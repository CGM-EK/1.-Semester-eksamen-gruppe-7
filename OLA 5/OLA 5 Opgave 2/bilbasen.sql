use bilbasen;

drop tables pristabel;
drop table autocampers;
drop tables forhandlere;



CREATE TABLE forhandlere (
cvr INT PRIMARY KEY,
forhandler TEXT,
adresse TEXT
);

CREATE TABLE autocampers (
    cvr					   INT ,
    makemodels             TEXT,
    links                  VARCHAR(500) PRIMARY KEY,
    beskrivelse            TEXT,
    Modelår                VARCHAR(500),
    X1_registrering        VARCHAR(500),
    Kilometertal           VARCHAR(500),
    Drivmiddel             VARCHAR(500),
    Brændstofforbrug       VARCHAR(500),
    CO2_udledning          VARCHAR(500),
    Euronorm               VARCHAR(500),
    Periodisk_afgift       VARCHAR(500),
    Ydelse                 VARCHAR(500),
    Acceleration           VARCHAR(500),
    Tophastighed           VARCHAR(500),
    Geartype               VARCHAR(500),
    Antal_gear             VARCHAR(500),
    Trækvægt               VARCHAR(500),
    Farve                  VARCHAR(500),
    Nypris                 VARCHAR(500),
    Kategori               VARCHAR(500),
    Type                   VARCHAR(500),
    Bagagerumsstørrelse    VARCHAR(500),
    Vægt                   VARCHAR(500),
    Bredde                 VARCHAR(500),
    Længde                 VARCHAR(500),
    Højde                  VARCHAR(500),
    Lasteevne              VARCHAR(500),
    Max_trækvægt_m_bremse  VARCHAR(500),
    Trækhjul               VARCHAR(500),
    Cylindre               VARCHAR(500),
    ABS_bremser            VARCHAR(500),
    ESP					   TEXT,
    Airbags				   TEXT,
    Tankkapacitet	   	   TEXT,
    Døre				   TEXT,
    FOREIGN KEY (`cvr`) REFERENCES forhandlere(`cvr`)
);

CREATE TABLE pristabel (
price_id INT AUTO_INCREMENT PRIMARY KEY,
links VARCHAR(150),
Scrape_date VARCHAR(50),
prices VARCHAR(20),
solgt VARCHAR(10),
FOREIGN KEY (links) REFERENCES autocampers(links)
);
