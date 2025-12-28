#lang north

-- @revision: e57f927cf191a83ba2a60d80ca95ff27
-- @parent: d96957547e7754c4d0072e0ce501da67
-- @description: Fix the revision table.
-- @up {
alter table revisions
add column source text not null;
-- }

-- @up {
alter table revisions
add column rendering text;
-- }

-- @up {
alter table revisions
drop column time;
-- }

-- @down {
alter table revisions
drop column source;
-- }

-- @down {
alter table revisions
drop column rendering;
-- }

-- @down {
alter table revisions
add column time;
-- }
