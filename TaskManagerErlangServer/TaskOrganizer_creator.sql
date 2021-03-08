CREATE database if not Exists TaskOrganizer;

 

use TaskOrganizer;

CREATE TABLE IF NOT EXISTS boards (
    board_title VARCHAR(255) PRIMARY KEY,
    last_update_time VARCHAR(255) not null
);


CREATE TABLE IF NOT EXISTS tasks (
    board_title VARCHAR(255) NOT NULL,
    task_description VARCHAR(255) NOT NULL,
    expiration_date DATE,
    stage_id INT, -- From 0 to 3
    task_title VARCHAR(255) NOT NULL,
    author VARCHAR(255) NOT NULL,
    'type' VARCHAR(255) NOT NULL,
    last_update_time VARCHAR(255) not null,

    primary key (task_title, board_title),
    foreign key (board_title)
        references boards(board_title)
        on update cascade
        on delete cascade
);

Create table if not exists recovery_hosts(
	host VARCHAR(255) NOT NULL ,
    down_time VARCHAR(255) NOT NULL,
    PRIMARY KEY (host)
);