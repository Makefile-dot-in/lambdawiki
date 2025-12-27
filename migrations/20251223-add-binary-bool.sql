#lang north

-- @revision: 486b10e8ad826870055bec81fceeb494
-- @parent: 8c2368e777268d36de32a299d2b69bfb
-- @description: Adds a binary boolean.
-- @up {
alter table content_types
add column is_binary boolean not null default false;
-- }

-- @down {
alter table content_types
drop column is_binary;
-- }
