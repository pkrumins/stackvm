create table users (
    id integer primary key,
    name text,
    hash text
);

create table vms (
    id integer primary key,
    name text,
    engine text,
    filename text, -- filename for qemu
    host text, -- host:port for vmware
    owner integer
);

create table processes (
    host text primary key,
    vm integer,
    engine text,
    pid integer
);

create table permissions (
    id integer primary key,
    vm integer,
    user integer,
    access integer
);

