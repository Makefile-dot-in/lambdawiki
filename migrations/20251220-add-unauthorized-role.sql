#lang north

-- @revision: cc243231e46727ba49948b4fba63d8d7
-- @parent: 7b19d5922543bdcbfa98f04b9617db87
-- @description: Alters some table.
-- @up {
insert into roles (id, name, corresponding_user, special) values
(3, 'Authenticated users', null, true);
-- }

-- @down {
delete from roles where id = 3;
-- }
