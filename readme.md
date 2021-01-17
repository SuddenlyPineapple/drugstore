# Drugstore

## How to run
Type in console `make run`, but before this rememeber to update PATH in `rel/sys.config` file 

## Endpoints

Offers endpoints:
- GET /offers/<id>
- POST /offers
- PUT /offers/<id>
- DELETE /offers

Orders endpoints:
- POST /orders
- GET /orders
- GET /orders/<id>


## Offer
```json
{
    "id": "text",
    "name": "text",
    "price": "float",
    "description": "text"
}
```

## Order 
```json
{
    "id": "text",
    "offerId": "text",
    "quantity": "int",
    "ammount": "float",
    "buyer": "text"
}
```
