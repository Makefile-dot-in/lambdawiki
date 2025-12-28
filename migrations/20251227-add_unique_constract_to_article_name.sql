#lang north

-- @revision: 9d1bb9f792b408db737490f1692769cc
-- @parent: 486b10e8ad826870055bec81fceeb494
-- @description: Alters some table.
-- @up {
alter table articles
add constraint unique_articles_name
unique (name);
-- }

-- @down {
alter table articles
drop constraint unique_articles_name;
-- }
