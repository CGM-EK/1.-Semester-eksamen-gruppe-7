use salling;

SELECT * FROM salling.stamdata_table;
drop table stamdata_table;
drop table food_waste_table;

CREATE TABLE food_waste_table (
 `store.id` VARCHAR(500),
 `offer.ean` BIGINT NOT NULL,
  `product.ean` BIGINT NOT NULL,
  `API_kald_tidspunkt` DATETIME,
  `offer.starttime` DATETIME,
  `offer.endtime` DATETIME,
  `offer.lastUpdate` DATETIME,
  `timer_tilbud_er_oppe` DECIMAL(10,2),
  `offer.currency` VARCHAR(3),
  `offer.newPrice` DECIMAL(10,2),
  `offer.originalPrice` DECIMAL(10,2),
  `offer.discount` DECIMAL(10,2),
  `offer.percentDiscount` DECIMAL(10,3),
  `offer.stock` DECIMAL(10,3),
  `offer.stockunit` VARCHAR(50),
  `product.categories.da` VARCHAR(500),
  `product.categories.en` VARCHAR(500),
  `product.description` VARCHAR(500),
  `product.image` VARCHAR(500),
  FOREIGN KEY (`store.id`) REFERENCES stamdata_table(`store.id`)
);

CREATE TABLE stamdata_table (
`store.id` VARCHAR(500) PRIMARY KEY,
`store.brand` VARCHAR(50),
`store.name` VARCHAR(200),
`store.address.city` VARCHAR(100),
`store.address.country` VARCHAR(2),
`store.address.extra` VARCHAR(100),
`store.address.street` VARCHAR(300),
`store.address.zip` BIGINT not NULL
);
