#lang north

-- @revision: b7e588ef248ca365e6f02fd57f939d98
-- @description: Creates the users table.
-- @up {
create table users(
       id bigint not null primary key,
       username text unique not null,
       pwhash bytea not null
);
-- }

-- @up {
create table roles(
       id bigint not null primary key,
       name text not null,
       -- not null for roles that are dependent on users
       corresponding_user bigint references users,
       -- true if this role is built into the system.
       special boolean not null default false
);
-- }

-- @up {
create table users_roles(
       userid bigint references users on delete cascade,
       roleid bigint references roles on delete cascade,
       primary key (userid, roleid)
);
-- }

-- @up {
create type permtype as enum('allow', 'deny');
-- }

-- @up {
create table user_permissions(
       id bigint not null primary key,
       category text not null,
       name text not null,
       performer bigint not null references roles on delete cascade,
       subject bigint not null references roles on delete cascade,
       perm permtype not null,
       priority integer not null default 50,
       unique (category, name, performer, subject)
);
-- }

-- @up {
insert into users (id, username, pwhash) values
(0, 'Root', '$argon2id$v=19$m=6000,t=1,p=4$lcA4j6e5vLgruyHXSJI/oA$6E3pR/SCMgsbwSMmPlMpgEBp5qcqjKcIUqMDebkLcCs');
-- }

-- @up {
-- These roles are built-in. ID is normally a snowflake,
-- so they won't clash with any user-defined roles.
insert into roles (id, name, corresponding_user, special) values
-- automatically considered to encompass all users
(0, 'All users', null, true),
-- when used as the subject in a permission, indicates the subject is
-- the user performing the operation
(1, 'Self', null, true),
-- the corresponding role for the root user. analogous to the root
-- user on Unix systems, this role has every permission
(2, 'Root', 0, true);
-- }

-- @up {
insert into users_roles (userid, roleid) values (0, 2);
-- }

-- @down {
drop table users;
-- }

-- @down {
drop table roles;
-- }

-- @down {
drop table users_roles;
-- }

-- @down {
drop type permtype;
-- }

-- @down {
drop table user_permissions;
-- }
