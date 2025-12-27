#lang north

-- @revision: 8eabb7d21743d5f8a6fd8624614fe647
-- @parent: cc243231e46727ba49948b4fba63d8d7
-- @description: Change source to bytea so it can contain images
-- @up {
alter table articles
alter column source type bytea
using source::bytea;
-- }

-- @down {
alter table articles
alter column source type text
using text::bytea;
-- }
