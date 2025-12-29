#lang north

-- @revision: 760d53e3dc9ce864b39aace8962cb7a7
-- @parent: e57f927cf191a83ba2a60d80ca95ff27
-- @description: Changes revision source to be bytea
-- @up {
alter table revisions
alter column source
type bytea
using source::bytea;
-- }

-- @down {
alter table revisions
alter column source
type text
using encode(source, 'escape');
-- }
