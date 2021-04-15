# Time series database
A time series database written in `Haskell` that uses `servant` and `acid-state` to persist and serve data. The client is in `elm` and it can be used to explore the features of the database.

This project was the topic of my bachelor thesis as to research a methodology for implementing a time series database using functional programming. The database is in RAM memory and it uses a specific data schema with appropriate indexes to store and query the data.

## Screenshots
![Screenshot from 2021-04-15 12-58-17](https://user-images.githubusercontent.com/39745825/114858844-47f3cc00-9dea-11eb-9ab1-d6dc9889eeeb.png)

## Setup
In order to run the database and the client, the easiest way is by using `docker-compose`:
```
docker-compose up --build
```
Or manually:
* Database (`stack` is required):
  ```
  make setup_server   # stack setup
  make build_and_run_server   # stack build and exec
  ```
* Client (`elm` is required):
  ```
  make run_client   # elm reactor
  ```
  or create a static `index.html` file in client/ directory
  ```
  make create_static_html
  ```

## Features
* Insert new data
* Delete existing data
* Update existing data
* Query data
  * In a time interval
  * Specific timestamp / tag
  * Aggregate by values ('average', 'sum', 'min', 'max', 'count')
  * Group by timestamp / tag
  * Sort ascending / descending
  * Limit return entries

## Inspiration
The data schema was heavily inspired by InfluxDB.
