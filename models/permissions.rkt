#lang racket
(require threading
         sql
         "../util/db.rkt"
         "../util/snowflake.rkt")

(provide
 (contract-out
  [has-user-permission? (-> symbol? (or/c #f snowflake?) snowflake? boolean?)]
  [has-article-permission? (-> symbol? (or/c #f snowflake?) snowflake? boolean?)]
  [has-permission-for-class? (-> symbol? (or/c #f snowflake?) snowflake?
                                 boolean?)]
  [all-role snowflake?]
  [self-role snowflake?]
  [root-role snowflake?]
  [auth-role snowflake?]
  [all-class snowflake?]))

(define symbol-to-category-and-name
  (λ~> symbol->string (string-split "/") (apply values _)))

(define all-role 0)
(define self-role 1)
(define root-role 2)
(define auth-role 3)

(define (has-user-permission? perm performer-user-id subject-user-id)
  (define-values (category name) (symbol-to-category-and-name perm))
  (~> (query-maybe-value
       (select
        #:from
        (left-join
         (left-join
          (as users_roles performer_users_roles)
          user_permissions
          #:on (= performer_users_roles.roleid user_permissions.performer))
         (as users_roles subject_users_roles)
         #:on (= subject_users_roles.roleid user_permissions.subject))
        #:where
        ; if the user has the root role, then they are allowed to do anything
        (or (ScalarExpr:AST
             ,(if performer-user-id
                  (scalar-expr-qq
                   (and (= performer_users_roles.userid ,performer-user-id)
                        (= performer_users_roles.roleid ,root-role)))
                  (scalar-expr-qq (ScalarExpr:INJECT ,"false"))))
            
            (and (or (and (or (ScalarExpr:AST
                               ; if the user has been specified, then any permissions that have
                               ; either the auth role as the subject or where the performer matches apply.
                               ,(if performer-user-id
                                    (scalar-expr-qq
                                     (or (= performer_users_roles.userid ,performer-user-id)
                                         (= user_permissions.performer ,auth-role)))
                                    (scalar-expr-qq (ScalarExpr:INJECT ,"false"))))

                              ; of course, any permission that has been granted to all users also applies.
                              (= user_permissions.performer ,all-role))
                          
                          ; the subject also needs to match, of course
                          ; you can't perform tasks on unauthorized users, so we include the auth role as well
                          (or
                           (in user_permissions.subject
                               #:values ,all-role ,auth-role)
                           (= users_roles.userid ,subject-user-id)))

                     (ScalarExpr:AST
                      ; if the performer is the same as the subject, then we have to check
                      ; for the self role as well.
                      ,(if (equal? performer-user-id subject-user-id)
                           (scalar-expr-qq (= user_permissions.subject ,self-role))
                           (scalar-expr-qq (ScalarExpr:INJECT ,"false")))))

                 (is-not-null user_permissions.perm)
                 (= user_permissions.category ,category)
                 (= user_permissions.name ,name)))
        #:order-by user_permissions.priority #:desc user_permissions.id
        #:limit 1
        #:values (cast user_permissions.perm text)))

      ; here we map #f (meaning no value) to "deny"
      (or "deny")
      ; if we got NULL from the query, that means the first part of the WHERE clause was true, since
      ; the second clause forbids permtype to be NULL. so we interpret such a result the same as an "allow"
      (member (list sql-null "allow"))
      false? not))

(define all-class 0)

(define (has-article-permission? perm performer-user-id article-id)
  (define-values (category name) (symbol-to-category-and-name perm))
  (~> (query-maybe-value
       (select
        #:from
        (left-join
         (left-join
          users_roles
          article_permissions
          #:on (= users_roles.userid article_permissions.performer))
         article_classes
         #:on (= article_classes.class article_permissions.subject))

        #:where
        ; if the user has the root role, then they are allowed to do anything
        (or (ScalarExpr:AST
             ,(if performer-user-id
                  (scalar-expr-qq
                   (and (= users_roles.userid ,performer-user-id)
                        (= users_roles.roleid ,root-role)))
                  (scalar-expr-qq (ScalarExpr:INJECT ,"false"))))

            (and (or (ScalarExpr:AST
                      ; if the user has been specified, then any permissions that have
                      ; either the auth role as the performer or where the performer matches apply.
                      ,(if performer-user-id
                           (scalar-expr-qq
                            (or (= users_roles.userid ,performer-user-id)
                                (= article_permissions.performer ,auth-role)))
                           (scalar-expr-qq (ScalarExpr:INJECT ,"false"))))

                     ; of course, any permission that has been granted to all users also applies.
                     (= article_permissions.performer ,all-role))

                 ; the subject also needs to match, of course
                 (or (= article_classes.article ,article-id)
                     (= article_permissions.subject ,all-class))
                 (is-not-null article_permissions.perm)
                 (= article_permissions.category ,category)
                 (= article_permissions.name ,name)))
        #:order-by article_permissions.priority #:desc article_permissions.id
        #:limit 1
        #:values (cast article_permissions.perm text)))
      (or "deny")
      (member (list sql-null "allow"))
      false? not))

(define (has-permission-for-class? perm performer-user-id article-class)
  (define-values (category name) (symbol-to-category-and-name perm))
  (~> (query-maybe-value
       (select
        #:from
        (left-join
         users_roles
         article_permissions
         #:on (= users_roles.userid article_permissions.performer))
        #:where
        ; if the user has the root role, then they are allowed to do anything
        (or (ScalarExpr:AST
             ,(if performer-user-id
                  (scalar-expr-qq
                   (and (= users_roles.userid ,performer-user-id)
                        (= users_roles.roleid ,root-role)))
                  (scalar-expr-qq (ScalarExpr:INJECT ,"false"))))

            (and (or (ScalarExpr:AST
                      ; if the user has been specified, then any permissions that have
                      ; either the auth role as the performer or where the performer matches apply.
                      ,(if performer-user-id
                           (scalar-expr-qq
                            (or (= users_roles.userid ,performer-user-id)
                                (= article_permissions.performer ,auth-role)))
                           (scalar-expr-qq (ScalarExpr:INJECT ,"false"))))

                     ; of course, any permission that has been granted to all users also applies.
                     (= article_permissions.performer ,all-role))

                 ; the subject also needs to match, of course
                 (in article_permissions.subject
                     #:values ,article-class ,all-class)
                 (is-not-null article_permissions.perm)
                 (= article_permissions.category ,category)
                 (= article_permissions.name ,name)))
        #:order-by article_permissions.priority #:desc article_permissions.id
        #:limit 1
        #:values (cast article_permissions.perm text)))
      (or "deny")
      (member (list sql-null "allow"))
      false? not))
