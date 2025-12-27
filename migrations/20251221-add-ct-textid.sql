#lang north

-- @revision: 8c2368e777268d36de32a299d2b69bfb
-- @parent: 8eabb7d21743d5f8a6fd8624614fe647
-- @description: Adds a textid column to content types.
-- @up {
alter table content_types
add column textid text not null unique;
-- }

-- @down {
alter table content_types drop column textid;
-- }
