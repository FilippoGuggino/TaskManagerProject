CREATE database if not Exists TaskOrganizer;

 

use TaskOrganizer;

 

 

 

CREATE TABLE IF NOT EXISTS boards (
    board_title VARCHAR(255) PRIMARY KEY,
    last_update_time VARCHAR(255) not null
);

 

 

 

CREATE TABLE IF NOT EXISTS stages (
    stage_id INT AUTO_INCREMENT,
    stage_title VARCHAR(255) NOT NULL,
    board_title VARCHAR(255),
    last_update_time VARCHAR(255) not null,
    primary key (stage_id, board_title),
    foreign key (board_title)
        references boards(board_title)
        on update cascade
        on delete cascade
);

 

CREATE TABLE IF NOT EXISTS tasks (
    task_id INT AUTO_INCREMENT,
    task_description VARCHAR(255) NOT NULL,
    expiration_date DATE,
    stage_id INT,
    last_update_time VARCHAR(255) not null,
    primary key (task_id),
    foreign key (stage_id)
        references stages(stage_id)
        on update cascade
        on delete cascade
);

Create table if not exists recovery_hosts(
	host VARCHAR(255) NOT NULL ,
    down_time VARCHAR(255) NOT NULL,
    PRIMARY KEY (host)
);