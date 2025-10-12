from pydantic import BaseModel

from fastapi import FastAPI

app = FastAPI()


@app.get("/")
async def index():
    return {"data": "Hello, World!"}


class Item(BaseModel):
    name: str
    price: float
    is_offer: bool | None = None


@app.get("/item/{item_id}")
async def item_get(item_id: int, q: str | None = None):
    """
    `item_id` - path parameter
    `q`       - query string
    """

    return {"item_id": item_id, "q": q}


@app.put("/item/{item_id}")
async def item_put(item_id: int, item: Item):
    """
    `item_id` - path parameter
    `item`    - request body 👍🏻
    """

    return {"item_id": item_id, "item_name": item.name}
