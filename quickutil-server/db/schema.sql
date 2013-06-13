CREATE TABLE utility (
    id BIGINT UNSIGNED NOT NULL AUTO_INCREMENT,
    name VARCHAR(256) NOT NULL,
    version FLOAT UNSIGNED NOT NULL,
    PRIMARY KEY (id),
    UNIQUE KEY (name, version),
    KEY (name)
) ENGINE=InnoDB DEFAULT CHARSET=binary;

CREATE TABLE utility_stats (
    utility_id BIGINT UNSIGNED NOT NULL,
    download_count INT UNSIGNED NOT NULL DEFAULT 1,
    PRIMARY KEY (utility_id)
) ENGINE=InnoDB;
