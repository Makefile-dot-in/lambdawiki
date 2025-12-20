#lang north

-- @revision: d2482e122e211a0138aeff7c35b41309
-- @parent: b7e588ef248ca365e6f02fd57f939d98
-- @description: Creates the session table.
-- @up {
create table sessions(
       session_id bytea primary key,
       user_id bigint not null references users on delete cascade,
       expires_at timestamptz,
       created_at timestamptz,
       ip_address text,
       user_agent text
);
-- }

-- @down {
drop table sessions;
-- }
