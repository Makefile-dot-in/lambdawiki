package require tdbc::postgres
package require Tk

tdbc::postgres::connection create db {*}$argv

set epoch 1765925858
proc new_snowflake {} {
    global snowcounter
    global epoch

    expr {([clock seconds] - $epoch) << 22
          | ([incr snowcounter] & ((1 << 22) - 1))}
}

proc snowflake_date {s} {
    global epoch
    expr {($s >> 22) + $epoch}
}

proc hash_password {password} {
    set random [open /dev/random rb]
    set salt [read $random 12]
    set argon2process [open [list | argon2 $salt -id -t 1 -p 4 -k 6000 -e] r+b]
    puts -nonewline $argon2process $password
    close $argon2process write
    set pwhash [string trim [read $argon2process]]
    close $argon2process
    set pwhash
}

proc insert_user {username password} {
    db transaction {
        set new_user_id [new_snowflake]
        set pwhash [hash_password $password]
        db allrows {
            INSERT INTO users (id, username, pwhash)
            VALUES (:new_user_id, :username, :pwhash)
        }

        set new_role_id [new_snowflake]
        db allrows {
            INSERT INTO roles(id, name, corresponding_user)
            VALUES (:new_role_id, :username, :new_user_id)
        }

        db allrows {
            INSERT INTO users_roles(userid, roleid)
            VALUES (:new_user_id, :new_role_id)
        }
    }
}

proc load_users {w} {
    $w delete [$w children {}]
    db foreach -as lists row {SELECT id, username FROM users ORDER BY username} {
        puts $row
        lassign $row id username
        $w insert {} end \
            -id $id \
            -text $username \
            -values [list $id [clock format [snowflake_date $id]]]
    }
}

namespace eval forms {
    variable counter 0
}
proc show_form {args} {
    array set a $args
    set var ::forms::[incr ::forms::counter]
    if {[info exists a(-defaults)]} {
        array set $var $a(-defaults)
    }
    toplevel .form
    wm resizable .form no no
    wm title .form $a(-title)

    ttk::frame .form.c
    pack .form.c -expand 1 -fill both

    foreach {label name type} $a(-form) {
        ttk::label .form.${name}lbl -text $label
        set rest [lassign $type kind]
        switch $kind {
            text {
                ttk::entry .form.${name}ent {*}$rest \
                    -textvariable ${var}($name)
            }
            pq {
                prompt_query .form.${name}ent {*}$rest \
                    -textvariable ${var}($name)
            }
            choice {
                ttk::combobox .form.${name}ent {*}$rest \
                    -textvariable ${var}($name)
            }
            default {
                error "unknown kind: $kind"
            }
        }
        grid .form.${name}lbl .form.${name}ent -in .form.c
    }

    set a(-cb) [list {*}$a(-cb)]

    bind .form <Destroy> [list unset -nocomplain $var]

    ttk::button .form.ok -text $a(-oklbl) \
        -command [string map [list @CB@ $a(-cb) @VAR@ $var] {
            @CB@ [array get @VAR@]
            destroy .form
        }]

    ttk::button .form.cancel -text Cancel \
        -command { destroy .form }

    grid .form.ok .form.cancel -in .form.c
}

proc prompt_query_open {w stmt} {
    wm deiconify $w.win
    $w.win.list delete [$w.win.list children {}]
    $stmt foreach -as dicts row {
        $w.win.list insert {} end \
            -id [dict get $row id] \
            -values [lmap col [$w.win.list cget -columns] {dict get $row $col}]
    }
}

proc prompt_query_select {w variable intvn} {
    upvar #0 $variable v
    upvar #0 $intvn intv
    lassign [$w.win.list selection] selid
    if {$selid eq {}} return
    wm withdraw $w.win
    set v [$w.win.list set $selid]
    array set intv $v
}

namespace eval pq {
    variable counter 0
}

proc prompt_query {w args} {
    array set a $args
    upvar #0 $a(-textvariable) tv
    set varname ::pq::[incr ::pq::counter]
    if {[info exists tv]} { array set $varname $tv }
    set stmt [uplevel 1 [list db prepare $a(-query)]]
    ttk::button $w -textvariable ${varname}($a(-labelcolumn)) -command \
        [list prompt_query_open $w $stmt]
    toplevel $w.win
    wm withdraw $w.win
    wm title $w.win $a(-title)
    wm protocol $w.win WM_DELETE_WINDOW [list wm withdraw $w.win]
    ttk::treeview $w.win.list \
        -columns [lmap {id _} $a(-columns) { set id }] \
        -selectmode browse \
        -show headings

    pack $w.win.list -fill both -expand 1
    foreach {id name} $a(-columns) { $w.win.list heading $id -text $name }

    bind $w.win.list <Double-1> [list prompt_query_select $w $a(-textvariable) $varname]
    bind $w <Destroy> [list $stmt close]\n[list unset -nocomplain $varname]
}

proc load_permissions {w table subjtable} {
    set query [string map [list @PERMTABLE@ $table @SUBJTABLE@ $subjtable] {
        SELECT
          permtable.id, permtable.category, permtable.name,
          permtable.performer, performer_role.name,
          permtable.subject, subject.name,
          permtable.priority,
          permtable.perm
        FROM @PERMTABLE@ AS permtable
        JOIN @SUBJTABLE@ AS subject ON subject = subject.id
        JOIN roles AS performer_role ON performer = performer_role.id
        ORDER BY permtable.priority DESC
    }]
    $w delete [$w children {}]
    db foreach -as lists row $query {
        lassign $row id
        $w insert {} end -id $id \
            -values [list {*}$row [clock format [snowflake_date $id]]]
    }
}

proc prompt_user {args} {
    array set a $args
    show_form \
        -title $a(-title) \
        -oklbl $a(-oklbl) \
        -cb $a(-cb) \
        -form {
            "Username" username text
            "Password" password password
        }
}

proc prompt_class {args} {
    show_form \
        {*}$args \
        -form {
            "Name"     name     text
            "Regex"    regex    text
            "Ordering" ordering text
        }
}

proc insert_class {res} {
    dict update res \
        name name \
        regex regex \
        ordering ordering \
        {}

    set id [new_snowflake]
    db transaction {
        db allrows {
            INSERT INTO classes
              (id, name, regex, ordering)
            VALUES
              (:id, :name, :regex, :ordering::integer)
        }
    }
}

proc load_classes {w} {
    $w delete [$w children {}]
    db foreach -as lists row {SELECT id, name, special, regex, ordering} {
        lassign $row id
        $w insert {} end \
            -id $id \
            -values $row
    }
}

proc prompt_permission {args} {
    array set a $args
    set mapping [list @SUBJECTBL@ $a(-subjecttable) \
                     @SUBJECTTITLE@ [list $a(-subjecttitle)]]
    show_form \
        {*}[array get a] \
        -form [string map $mapping {
            "Category"  category  text
            "Name"      name      text
            "Performer" performer {pq
                -query {SELECT id, name FROM roles}
                -title "Select a role"
                -columns {id "ID" name "Name"}
                -labelcolumn name
            }
            "Subject" subject {pq
                -query {SELECT id, name FROM @SUBJECTBL@}
                -title @SUBJECTTITLE@
                -columns {id "ID" name "Name"}
                -labelcolumn name
            }
            "Permission type" permtype {choice -values {allow deny}}
            "Priority" priority text
        }]
}

proc create_permission {tbl a} {
    set id [new_snowflake]
    dict update a \
        category category \
        name name \
        permtype permtype \
        priority priority \
        {}
    set performer [dict get $a performer id]
    set subject [dict get $a subject id]
    db transaction {
        db allrows [string map [list @TABLE@ $tbl] {
            INSERT INTO @TABLE@
              (id, category, name, performer, subject, perm, priority)
            VALUES
              (:id, :category, :name, :performer, :subject, :permtype, :priority)
        }]
    }
}

proc edit_permission {tbl id a} {
    dict update a \
        category category \
        name name \
        permtype permtype \
        priority priority \
        {}
    set performer [dict get $a performer id]
    set subject [dict get $a subject id]
    db transaction {
        db allrows [string map [list @TABLE@ $tbl] {
            UPDATE @TABLE@
            SET category = :category,
                name = :name,
                performer = :performer,
                subject = :subject,
                perm = :permtype,
                priority = :priority
            WHERE id = :id
        }]
    }
}

proc delete_permission {tbl id} {
    db transaction {
        db allrows [string map [list @TABLE@ $tbl] {
            DELETE FROM @TABLE@ WHERE id = :id
        }]
    }
}

proc row->dict {row} {
    set retval [dict create]
    foreach {key val} $row {
        dict set retval {*}$key $val
    }

    set retval
}

ttk::frame .c
ttk::notebook .nb
.nb add [ttk::frame .nb.users] -text "Users"

ttk::treeview .nb.users.list -columns {id created_at}
.nb.users.list heading id -text "User ID"
.nb.users.list heading created_at -text "Created at"

ttk::button .nb.users.load_users \
    -text "Load users" \
    -command {load_users .nb.users.list}

ttk::button .nb.users.create_user \
    -text "Create user" \
    -command {
        prompt_user \
            -title "Create a new user" \
            -oklbl "Create" \
            -cb insert_user
    }

.nb add [ttk::frame .nb.classes] -text "Classes"
ttk::treeview .nb.classes.list \
    -columns {id name special regex ordering} \
    -show headings \
    -displaycolumns {id name special regex ordering}

ttk::button .nb.classes.load \
    -text "Load classes"

pack .nb.classes.load -side left -anchor nw
pack .nb.classes.list -side top -expand yes -fill both -anchor nw
#pack .nb.classes.create_class -side left -anchor nw

pack .nb.users.list -side top -expand yes -fill both -anchor nw
pack .nb.users.load_users -side left -anchor nw
pack .nb.users.create_user -side left -anchor nw

.nb add [ttk::frame .nb.permissions] -text "Article permissions"
ttk::treeview .nb.permissions.list \
    -columns {
        id category name
        {performer id} {performer name}
        {subject id}   {subject name}
        priority permtype created_at
    } \
    -show headings \
    -displaycolumns {
        id category name
        {performer name} {subject name}
        priority permtype created_at
    }

.nb.permissions.list heading id -text ID
.nb.permissions.list heading category -text Category
.nb.permissions.list heading {performer name} -text Performer
.nb.permissions.list heading {subject name} -text Subject
.nb.permissions.list heading priority -text Priority
.nb.permissions.list heading permtype -text "Permission type"
.nb.permissions.list heading created_at -text "Created at"

ttk::button .nb.permissions.load \
    -text "Load permissions" \
    -command {load_permissions .nb.permissions.list article_permissions classes}

ttk::button .nb.permissions.create \
    -text "Add permission" \
    -command {
        prompt_permission \
            -title "Add a new permission" \
            -oklbl "Add" \
            -cb {create_permission article_permissions} \
            -subjecttable classes \
            -subjecttitle "Select a class"
    }

bind .nb.permissions.list <Double-1> {
    lassign [%W selection] selid
    if {$selid ne {}} {
        prompt_permission \
            -title "Edit permission $selid" \
            -oklbl "Edit" \
            -cb [list edit_permission article_permissions $selid] \
            -subjecttable classes \
            -subjecttitle "Select a class" \
            -defaults [row->dict [%W set $selid]]
    }
}

pack .nb.permissions.list -side top -expand yes -fill both -anchor nw
pack .nb.permissions.load -side left -anchor nw
pack .nb.permissions.create -side left -anchor nw

pack .nb -expand yes -fill both -in .c
pack .c -expand yes -fill both


