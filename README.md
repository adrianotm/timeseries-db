# Time series database
A time series database written in `Haskell` that uses `servant` and `acid-state` to persist and serve data. The client is in `elm` and it can be used to explore the features of the database.

This project was the topic of my bachelor thesis as to research a methodology for implementing a time series database using functional programming. The database is in RAM memory and it uses a specific data schema with appropriate indexes to efficiently store and query the data.

## Table of contents
<!--ts-->
   * [Screenshots](#screenshots)
   * [Setup](#setup)
     * [Docker](#docker)
     * [Manual](#manual)
   * [Usage](#usage)
     * [Insert data](#insert-data)
     * [Update data](#update-data)
     * [Delete data](#delete-data)
     * [Query data](#query-data)
   * [Inspiration](#inspiration)
<!--te-->

## Screenshots
![Screenshot from 2021-04-15 12-58-17](https://user-images.githubusercontent.com/39745825/114858844-47f3cc00-9dea-11eb-9ab1-d6dc9889eeeb.png)

## Setup

#### Docker
In order to run the database and the client, the easiest way is by using `docker-compose`:
```
docker-compose up --build
```
The client is available at `http://localhost:8080`.
#### Manual
* Database (`stack` is required):
 
  ```
  make setup_server   # stack setup
  make build_and_run_server   # stack build and exec
  ```
* Client (`elm` is required):
 
  ```
  make run_client   # elm reactor, open the src/Main.elm file
  ```
  or create a static `client/index.html` file
  
  ```
  make create_static_html
  ```
  
## Usage
The database listens on port 8081.

### Insert data
Route **POST /timeseries** - The new data should be in the body of the request. Example:
```
[
  {
    "timestamp": 16184889510000,
    "tag": "foo",
    "value": 10
  },
  ...
]
```
Inserting data that already exists results in an error.

### Update data
Route **PUT /timeseries** - The entries that should be updated are passed in the body of the request. Example:
```
[
  {
    "timestamp": 16185881510200,
    "tag": "bar",
    "value": 55
  },
  ...
]
```
Updating data that does not exist in the database results in an error.

### Delete data
Route **DELETE /timeseries** - If the body of the request is empty, the whole database is cleared. To delete specific entries, pass the entries in the body. Example:
```
[
  {
    "timestamp": 16185881510200,
    "tag": "bar"
  },
  ...
]
```
Deleting data that does not exist in the database results in an error.

### Query data
Route **POST /timeseries/query** - A query should be passed in the body of the request. Query parameters:
```haskell
{
  gt :: Int,    -- timestamp greater then
  ge :: Int,    -- timestamp greater or equal then (mutually exclusive with 'gt')
  lt :: Int,    -- timestamp less then
  le :: Int,    -- timestamp less or equal then (mutually exclusive with 'lt')
  tsEq :: Int,    -- timestamp equal (mutually exclusive with any other timestamp parameter)
  tagEq :: String,    -- tag equal
  aggFunc :: "count" | "avg" | "sum" | "min" | "max",   -- aggregate data
  groupBy :: "tag" | "timestamp",   -- group by tag or timestamp, in combination with 'aggFunc'
  sort :: "asc" | "desc",   -- sort by timestamp ascending or descending
  limit :: int    -- limit entries in the result
}
```

## Inspiration
The data schema was heavily inspired by InfluxDB.
