#lang at-exp s-exp "format.rkt"
@prelude[
(define (vienskaitlis? num)
  (and (equal? (modulo num 10) 1)
       (not (equal? (modulo num 100) 11))))
]
@error-short[(no-handler)]{Nav atrasts}
@error-description[(no-handler)]{Pieprasītais resurss nav atrasts}
@error-short[(article-not-found)]{Nav atrasts}
@error-description[(article-not-found)]{Pieprasītais raksts nav atrasts}
@error-short[(unauthorized)]{Nav autorizācijas}
@error-description[(unauthorized)]{Jums nav autorizācijas piekļūt šim resursam}
@error-short[(internal-error)]{Iekšēja servera kļūda}
@error-description[(internal-error)]{Jūsu pieprasījums nevarēja tikt apstrādāts iekšējas servera kļūdas dēļ}
@error-short[(revision-not-found)]{Nav atrasts}
@error-description[(revision-not-found)]{Pieprasītās izmaiņas nav atrastas.}

@search-label[()]{Meklēt}
@useful-links[()]{Noderīgas saites}
@search-go[()]{meklēt}
@login-title[()]{Pieslēgties}
@login-submit[()]{Pieslēgties}
@login-username[()]{Lietotājvārds}
@login-password[()]{Parole}
@invalid-username-or-password[()]{Nederīgs lietotājvārds vai parole}
@signout-link[()]{Atteikties}
@login-link[()]{pieslēgties}
@login-header[(,wiki-name)]{Pieslēgties @wiki-name}
@login-status[(#f)]{nav pieslēdzies}
@login-status[(,name)]{pieslēdzies kā @name}
@mandatory-field[()]{Nepieciešams lauks}
@create-article[()]{Izveidot rakstu}
@edit-article[()]{Rediģēt rakstu}
@article-title[()]{Virsraksts}
@article-content-type[()]{Tips}
@article-upload[()]{Augšupielādēt}
@article-create-submit[()]{Izveidot rakstu}
@article-edit-submit[()]{Rediģet rakstu}
@article-class[()]{Klase}
@article-content[()]{Saturs}
@article-revisions[(,title)]{Raksta @title izmaiņas}
@article-revisions-time[()]{Izveidots}
@article-revisions-author[()]{Autors}
@article-revisions-actions[()]{Darbības}
@article-revisions-limit-label[()]{Izmaiņas lapā}
@page-limit-submit[()]{Lietot}
@page-next[()]{nākamā lapa}
@page-prev[()]{iepriekšējā lapa}
@article-revisions-action-view[()]{skatīt}
@found-revisions[(#f)]{Izmaiņas nav atrastas}
@found-revisions[(,(? vienskaitlis?))]{Atrasta 1 izmaiņa}
@found-revisions[(,n)]{Atrastas @n izmaiņas}
@article-links-page[()]{raksts}
@article-links-revisions[()]{raksta vēsture}
@article-links-edit[()]{rediģēt}
@invalid-title-for-class[()]{Rakstus ar šo klasi nevar izveidot ar šādu virsrakstu}
@reference-title[()]{Atsauces}
@title-must-be-unique[()]{Virsrakstam jābūt unikālam}
@article-create-sidelink[()]{Izveidot rakstu}
@table-of-contents[()]{Satura radītājs}
@article-introduction[()]{Ievads}
@rendering-error[()]{Renderēšanas kļūda}
@revision-title[(,number ,author)]{Lietotāja @author izmaiņa @number}
@class-add-button[()]{Pievienot klasi}
@found-articles[(#f)]{Neviens raksts nav atrasts}
@found-articles[(,(? vienskaitlis?))]{Atrasts 1 raksts}
@found-articles[(,count)]{Atrasti @count raksti}
@article-search-limit-label[()]{Raksti lapā}
@search-results[(,query)]{Vaicājuma @query rezultāti}
@search-article-name[()]{Virstaksts}
@search-article-type[()]{Tips}
@all-articles[()]{Rakstu uzskaitījums}
@article-delete-button[()]{Dzēst rakstu}
