## Meatbarrrr

## Dependencies

Sqlite 3

## Building the project

Building from source: `stack build`

Running the test suite: `stack test`

Starting the web server: `stack exec meatbar-exe`

## Querying the API

Here's some handy curl commands:

```
curl http://localhost:8081/api/people
curl http://localhost:8081/api/meatbars/consumption
curl http://localhost:8081/api/meatbars/streaks
curl http://localhost:8081/api/meatbars/activity
curl http://localhost:8081/api/docs
```


## API Endpoint Documentation


## GET /people

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[  
  {  
    "name":"Liz Lemon",
    "id":1
  },
  {  
    "name":"Tracy Jordan",
    "id":2
  }
]
```



## GET /meatbars/consumption

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[  
  {  
    "meatbar":{  
      "name":"lobster",
      "id":1
    },
    "eater":{  
      "name":"Liz Lemon",
      "id":1
    },
    "dateEaten":"1999-12-31T23:59:00.000000000000Z",
    "id":1
  },
  {  
    "meatbar":{  
      "name":"raccoon",
      "id":1
    },
    "eater":{  
      "name":"Tracy Jordan",
      "id":2
    },
    "dateEaten":"2000-01-01T00:00:00.000000000000Z",
    "id":2
  }
]
```




## GET /meatbars/streaks

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[  
  {  
    "streakDays":3,
    "endDate":"1999-12-31T23:59:00.000000000000Z",
    "startDate":"1999-12-31T23:59:00.000000000000Z",
    "barsEaten":14
  },
  {  
    "streakDays":5,
    "endDate":"1999-12-31T23:59:00.000000000000Z",
    "startDate":"1999-12-31T23:59:00.000000000000Z",
    "barsEaten":21
  }
]
```



## GET /meatbars/activity

#### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[  
  {  
    "mostActiveDay":3,
    "month":1,
    "eatenBars":[  
      {  
        "meatbar":{  
          "name":"lobster",
          "id":1
        },
        "eater":{  
          "name":"Liz Lemon",
          "id":1
        },
        "dateEaten":"1999-12-31T23:59:00.000000000000Z",
        "id":1
      },
      {  
        "meatbar":{  
          "name":"raccoon",
          "id":1
        },
        "eater":{  
          "name":"Tracy Jordan",
          "id":2
        },
        "dateEaten":"2000-01-01T00:00:00.000000000000Z",
        "id":2
      }
    ]
  },
  {  
    "mostActiveDay":12,
    "month":4,
    "eatenBars":[  
      {  
        "meatbar":{  
          "name":"lobster",
          "id":1
        },
        "eater":{  
          "name":"Liz Lemon",
          "id":1
        },
        "dateEaten":"1999-12-31T23:59:00.000000000000Z",
        "id":1
      },
      {  
        "meatbar":{  
          "name":"raccoon",
          "id":1
        },
        "eater":{  
          "name":"Tracy Jordan",
          "id":2
        },
        "dateEaten":"2000-01-01T00:00:00.000000000000Z",
        "id":2
      }
    ]
  }
]
```

