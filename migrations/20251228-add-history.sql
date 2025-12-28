#lang north

-- @revision: d96957547e7754c4d0072e0ce501da67
-- @parent: 9d1bb9f792b408db737490f1692769cc
-- @description: Creates the revision table.
-- @up {
create table revisions(
       id bigint primary key,
       article bigint not null references articles,
       time timestamptz not null,
       userid bigint references users,
       anon_ip text
);
-- }

-- @down {
drop table revisions;
-- }
