#lang north

-- @revision: 7b19d5922543bdcbfa98f04b9617db87
-- @parent: d2482e122e211a0138aeff7c35b41309
-- @description: Creates tables for articles
-- @up {
-- classes of articles 
create table classes(
       id bigint primary key,
       name text not null,
       -- true if this class is built into the system
       special boolean not null default false,
       -- if not null, a regex that all articles with this class must abide by
       regex text,
       -- number to order article classes by when displaying it
       ordering integer not null default 0
);
-- }

-- @up {
create table content_types(
       id bigint primary key,
       name text not null
);
-- }

-- @up {
create table articles(
       id bigint primary key,
       name text not null,
       content_type bigint references content_types,
       source text not null,
       -- cached rendering, if any
       rendering text
);       
-- }

-- @up {
create table article_classes(
       article bigint references articles on delete cascade,
       class bigint references classes on delete cascade,
       primary key (article, class)
);
-- }

-- @up {
create table article_permissions(
       id bigint not null primary key,
       category text not null,
       name text not null,
       performer bigint not null references roles on delete cascade,
       subject bigint not null references classes on delete cascade,
       perm permtype not null,
       priority integer not null default 50,
       unique (category, name, performer, subject)
);
-- }

-- @up {
insert into classes (id, name, special) values
(0, 'All articles', true);
-- }

-- @down {
drop table article_permissions;
-- }

-- @down {
drop table article_classes;
-- }

-- @down {
drop table articles;
-- }

-- @down {
drop table content_types;
-- }

-- @down {
drop table classes;
-- }
