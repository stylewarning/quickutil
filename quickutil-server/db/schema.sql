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

CREATE TABLE utility_categories (
    utility_id BIGINT UNSIGNED NOT NULL,
    category_name VARCHAR(256) NOT NULL,
    UNIQUE KEY (utility_id, category_name),
    KEY (utility_id),
    KEY (category_name)
) ENGINE=InnoDB DEFAULT CHARSET=binary;
