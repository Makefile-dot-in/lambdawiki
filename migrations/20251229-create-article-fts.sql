#lang north

-- @revision: c03f530762abd416fc70b2b9006fb535
-- @parent: 760d53e3dc9ce864b39aace8962cb7a7
-- @description: Creates a full text index for article names.
-- @up {
create index article_name_fts on articles
using gin (to_tsvector(name));
-- }

-- @down {
drop index article_name_fts;
-- }
