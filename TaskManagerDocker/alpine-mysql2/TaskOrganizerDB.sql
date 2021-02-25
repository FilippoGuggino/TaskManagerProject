use TaskOrganizer;

CREATE TABLE IF NOT EXISTS boards (
    board_title VARCHAR(255) PRIMARY KEY
);

CREATE TABLE IF NOT EXISTS stages (
    stage_id INT AUTO_INCREMENT,
    stage_title VARCHAR(255) NOT NULL,
    board_title VARCHAR(255),
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
    primary key (task_id),
    foreign key (stage_id)
    	references stages(stage_id)
    	on update cascade
    	on delete cascade
);
