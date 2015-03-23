## GET /random_numbers

#### Response:

- Status code 200

- Supported content types are:

    - `application/json`

- Response body as below.

```javascript
[4]
```

## POST /v1.0.0/card

#### GET Parameters:

- loud
     - **Values**: *true, false*
     - **Description**: Get the personalised card loudly. Default is false.


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

# Clients for free

## Given an API we can generate a Client
```haskell
client :: HasClient layout => Proxy layout -> Client layout                     
```

## This works via some magic
```haskell
class HasClient layout where
  type Client layout :: *
  clientWithRoute :: Proxy layout -> Req -> Client layout


  instance (HasClient a, HasClient b) => HasClient (a :<|> b) where               
    type Client (a :<|> b) = Client a :<|> Client b                               
    clientWithRoute Proxy req =                                                   
      clientWithRoute (Proxy :: Proxy a) req :<|>                                 
      clientWithRoute (Proxy :: Proxy b) req   
```

# Clients for free
