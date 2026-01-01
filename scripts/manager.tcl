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

proc create_user_form {} {
    global user_form

    toplevel .user_form
    wm withdraw .user_form
    wm resizable .user_form no no
    wm protocol .user_form WM_DELETE_WINDOW { wm withdraw .user_form }
    ttk::frame .user_form.c
    ttk::label .user_form.userlbl -text "Username"
    ttk::entry .user_form.user -textvariable user_form(username)
    ttk::label .user_form.pwlbl -text "Password"
    ttk::entry .user_form.pw -textvariable user_form(password) -show *
    ttk::button .user_form.ok -textvariable user_form(oklbl) \
        -command {
            wm withdraw .user_form
            {*}$user_form(cb) $user_form(username) $user_form(password)
        }
    ttk::button .user_form.cancel -text "Cancel" -command {
        wm withdraw .user_form
    }

    grid .user_form.userlbl .user_form.user -in .user_form.c
    grid .user_form.pwlbl .user_form.pw -in .user_form.c
    grid .user_form.ok .user_form.cancel -in .user_form.c
    
    pack .user_form.c -expand yes -fill both       
}

proc prompt_user {callback title oklbl} {
    global user_form
    wm title .user_form $title
    set user_form(cb) $callback
    set user_form(oklbl) $oklbl
    wm deiconify .user_form
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

proc prompt_query_select {w variable} {
    upvar #0 $variable v
    lassign [$w.win.list selection] selid
    if {$selid eq {}} return
    wm withdraw $w.win
    array set v [$w.win.list set $selid]
}

proc prompt_query {w query columns title variable lblrow} {
    set stmt [uplevel 1 [list db prepare $query]]
    ttk::button $w -textvariable ${variable}($lblrow) -command \
        [list prompt_query_open $w $stmt]
    toplevel $w.win
    wm withdraw $w.win
    wm title $w.win $title
    wm protocol $w.win WM_DELETE_WINDOW [list wm withdraw $w.win]
    ttk::treeview $w.win.list \
        -columns [lmap {id _} $columns { set id }] \
        -selectmode browse \
        -show headings

    pack $w.win.list -fill both -expand 1
    foreach {id name} $columns { $w.win.list heading $id -text $name }

    bind $w.win.list <Double-1> [list prompt_query_select $w $variable]
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

proc create_permission_form {w varname subjectbl subjecttitle} {
    upvar #0 $varname var

    toplevel $w
    wm withdraw $w
    wm resizable $w no no
    wm protocol $w WM_DELETE_WINDOW [list wm withdraw $w]
    ttk::frame $w.c
    ttk::label $w.categorylbl -text "Category"
    ttk::entry $w.category -textvariable ${varname}(category)
    ttk::label $w.namelbl -text "Name"
    ttk::entry $w.name -textvariable ${varname}(name)
    ttk::label $w.performerlbl -text "Performer"
    prompt_query $w.performer \
        {SELECT id, name FROM roles} \
        {id "ID" name "Name"} \
        "Select a role" \
        ${varname}_performer \
        name
    ttk::label $w.subjectlbl -text "Subject"
    prompt_query $w.subject \
        "SELECT id, name FROM $subjectbl" \
        {id "ID" name "Name"} \
        $subjecttitle \
        ${varname}_subject \
        name

    ttk::label $w.permtypelbl -text "Permission type"
    ttk::combobox $w.permtype \
        -values {allow deny} \
        -textvariable ${varname}(permtype)

    ttk::label $w.prioritylbl -text "Priority"
    ttk::entry $w.priority \
        -textvariable ${varname}(priority)

    ttk::button $w.accept -textvariable ${varname}(oklbl)\
        -command [list apply {{w vn} {
            upvar #0 $vn var
            upvar #0 ${vn}_performer perf
            upvar #0 ${vn}_subject subj
            wm withdraw $w
            {*}$var(cb) [list {*}[array get var] \
                             performer [array get perf] \
                             subject [array get subj]]
        }} $w $varname]
    ttk::button $w.cancel -text "Cancel" \
        -command [list wm withdraw $w]

    grid $w.categorylbl $w.category -in $w.c
    grid $w.namelbl $w.name -in $w.c
    grid $w.performerlbl $w.performer -in $w.c
    grid $w.subjectlbl $w.subject -in $w.c
    grid $w.permtypelbl $w.permtype -in $w.c
    grid $w.prioritylbl $w.priority -in $w.c
    grid $w.accept $w.cancel -in $w.c

    pack $w.c -fill both -expand 1
}

proc prompt_permission {callback w var oklbl title data} {
    upvar #0 $var v
    upvar #0 ${var}_performer perf
    upvar #0 ${var}_subject subj
    array set d $data

    set v(cb) $callback
    set v(oklbl) $oklbl
    set v(category) $d(category)
    set v(name) $d(name)
    set perf(id) $d(performerid)
    set subj(id) $d(subjectid)
    set perf(name) $d(performername)
    set subj(name) $d(subjectname)
    set v(permtype) $d(permtype)
    set v(priority) $d(priority)

    wm title $w $title
    wm deiconify $w
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

ttk::frame .c
ttk::notebook .nb
.nb add [ttk::frame .nb.users] -text "Users"

create_user_form
ttk::treeview .nb.users.list -columns {id created_at}
.nb.users.list heading id -text "User ID"
.nb.users.list heading created_at -text "Created at"

ttk::button .nb.users.load_users \
    -text "Load users" \
    -command [list load_users .nb.users.list]

ttk::button .nb.users.create_user \
    -text "Create user" \
    -command {prompt_user insert_user "Create a new user" "Create"}

pack .nb.users.list -side top -expand yes -fill both -anchor nw
pack .nb.users.load_users -side left -anchor nw
pack .nb.users.create_user -side left -anchor nw

.nb add [ttk::frame .nb.permissions] -text "Article permissions"
ttk::treeview .nb.permissions.list \
    -columns {
        id category name performerid performername
        subjectid subjectname priority permtype created_at
    } \
    -show headings \
    -displaycolumns {id category name performername
        subjectname priority permtype created_at}

.nb.permissions.list heading id -text ID
.nb.permissions.list heading category -text Category
.nb.permissions.list heading performername -text Performer
.nb.permissions.list heading subjectname -text Subject
.nb.permissions.list heading priority -text Priority
.nb.permissions.list heading permtype -text "Permission type"
.nb.permissions.list heading created_at -text "Created at"

ttk::button .nb.permissions.load \
    -text "Load permissions" \
    -command {load_permissions .nb.permissions.list article_permissions classes}


create_permission_form .article_perm_form article_perm_form classes "Choose a class"

ttk::button .nb.permissions.create \
    -text "Add permission" \
    -command {
        prompt_permission \
            {create_permission article_permissions} \
            .article_perm_form \
            article_perm_form \
            "Add" \
            "Add a new permission" \
            {
                category ""
                name ""
                performerid ""
                performername ""
                subjectid ""
                subjectname ""
                permtype ""
                priority ""
            }
    }

bind .nb.permissions.list <Double-1> {
    lassign [%W selection] selid
    if {$selid ne {}} {
        prompt_permission \
            [list edit_permission article_permissions $selid] \
            .article_perm_form \
            article_perm_form \
            "Edit" \
            "Edit permission $selid" \
            [%W set $selid]
    }
}

pack .nb.permissions.list -side top -expand yes -fill both -anchor nw
pack .nb.permissions.load -side left -anchor nw
pack .nb.permissions.create -side left -anchor nw

pack .nb -expand yes -fill both -in .c
pack .c -expand yes -fill both


