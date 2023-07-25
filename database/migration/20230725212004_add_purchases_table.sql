-- migrate:up
CREATE TABLE purchases (
  id SERIAL PRIMARY KEY,
  title TEXT NOT NULL,
  price_in_cent INT NOT NULL,
  date DATE NOT NULL
);

-- migrate:down
DROP TABLE IF EXISTS purchases;
