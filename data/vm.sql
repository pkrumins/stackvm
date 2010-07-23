create table users (
    id integer primary key,
    name text,
    hash text
);

create table vms (
    id integer primary key,
    name text,
    filename text,
    engine text,
    owner integer,
    port integer,
    pid integer
);

create table permissions (
    id integer primary key,
    vm integer,
    user integer,
    access integer
);
