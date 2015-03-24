## POST /v1.0.0/card

#### GET Parameters:

- loud
     - **Values**: *true, false*
     - **Description**: Get the personalised card loudly. Default is false.
     - This parameter is a **flag**. This means no value is expected to be associated to this parameter.


#### Request:

- Supported content types are:

    - `application/x-www-form-urlencoded`
    - `application/json`

- Example: `application/x-www-form-urlencoded`

```
full_name=Hubert%20Cumberdale
```

- Example: `application/json`

```javascript
{"_nameFull":"Hubert Cumberdale"}
```

#### Response:

- Status code 201

- Supported content types are:

    - `application/json`

- If you use ?loud

```javascript
{"_cardBody":"HELLO, HUBERT CUMBERDALE!!1"}
```

- If you do not use ?loud

```javascript
{"_cardBody":"Hello, Hubert Cumberdale."}
```

## GET /v1.0.0/random_numbers

#### Response:

- Status code 200

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[4]
```


