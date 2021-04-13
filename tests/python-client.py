import urllib.request
import json

import subprocess
import time

import os
os.chdir(os.path.dirname(__file__))

def accounts_summary(tag):
    url = 'http://127.0.0.1:8080/accounts_summary?tag={}'.format(tag)
    answer = urllib.request.urlopen(url).read().decode()
    return json.loads(answer)

def get_positions(account_id):
    answer = urllib.request.urlopen("http://127.0.0.1:8080/positions").read().decode()
    return json.loads(answer)
    
def get_positions_with_pnls(account_id):
    answer = urllib.request.urlopen("http://127.0.0.1:8080/positions?pnl=true").read().decode()
    return json.loads(answer)

def place_order(side, symbol, quantity, at_price_type):
    url = 'http://127.0.0.1:8080/place_order?side={}&symbol={}&quantity={}&at_price_type={}'.format(side, symbol, quantity, at_price_type)
    answer = urllib.request.urlopen(url).read().decode()
    return json.loads(answer)

def place_fake_order(side, symbol, quantity, at_price_type):
    url = 'http://127.0.0.1:8080/place_fake_order?side={}&symbol={}&quantity={}&at_price_type={}'.format(side, symbol, quantity, at_price_type)
    answer = urllib.request.urlopen(url).read().decode()
    data = json.loads(answer)
    return data

def buy_quantity_for(symbol, amount):
    latest_price = get_latest_price(symbol)

    found_quantity = False
    quantity = math.floor(amount/latest_price)

    while not found_quantity:
        if quantity <= 0:
            return 0
        commission = place_fake_order("BUY", symbol, quantity, "MKT")["commission"]
        total_cost = quantity * latest_price + commission
        if total_cost <= amount:
            found_quantity = True
        else:
            quantity = quantity - 1

    return quantity

def cancel_order(request_id):
    url = 'http://127.0.0.1:8080/cancel_order?request_id={}'.format(request_id)
    answer = urllib.request.urlopen(url).read().decode()
    return json.loads(answer)

def open_orders():
    answer = urllib.request.urlopen("http://127.0.0.1:8080/open_orders").read().decode()
    return json.loads(answer)


if __name__ == "__main__":

    # at another command prompt, execute:
    # ./rest_server run port:8080 gateway:ib_paper
    
    accounts_summary = accounts_summary("NET_LIQUIDATION")
    print(json.dumps(accounts_summary, indent=4, sort_keys=True))

    main_account_id = list(accounts_summary["accounts"].keys())[0]
    print(main_account_id)

    # positions = get_positions(main_account_id)
    # print(json.dumps(positions, indent=4, sort_keys=True))

    # positions_with_pnl = get_positions_with_pnls(main_account_id)
    # print(json.dumps(positions, indent=4, sort_keys=True))

    # order = place_order ("buy", "T", 10, "midprice")
    # print(json.dumps(order, indent=4, sort_keys=True))

    # oo = open_orders()
    # oo = oo["accounts"][main_account_id]["open_orders"]
    # print(json.dumps(oo, indent=4, sort_keys=True))

    # for symbol in oo:
    #     request_id = oo[symbol]["request_id"]
    #     result = cancel_order(request_id)
    #     print(result)

    oo = open_orders()
    oo = oo["accounts"][main_account_id]["open_orders"]
    print(json.dumps(oo, indent=4, sort_keys=True))