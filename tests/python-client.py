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

    positions_with_pnl = get_positions_with_pnls(main_account_id)
    positions_with_pnl = positions_with_pnl["accounts"][main_account_id]["positions"]
    total_pnl = 0
    total_stake = 0
    for pos in positions_with_pnl:
        total_stake = total_stake + positions_with_pnl[pos]["open_value"]
        total_pnl = total_pnl + positions_with_pnl[pos]["pnl_unrealized"]
    
    positions_with_pnl.update({"total_pnl" : total_pnl,
                               "total_stake" : total_stake,
                               "appreciation" : total_pnl/total_stake})

    print(json.dumps(positions_with_pnl, indent=4, sort_keys=True))


    #order = place_order ("buy", "VZIO", 44, "midprice")
    #print(json.dumps(order, indent=4, sort_keys=True))

    oos = open_orders()
    oos = oos["accounts"][main_account_id]["open_orders"]
    print(json.dumps(oos, indent=4, sort_keys=True))

    #for oo in oos:
    #    request_id = oo["request_id"]
    #    result = cancel_order(request_id)
    #    print(result)

    #oos = open_orders()
    #oos = oos["accounts"][main_account_id]["open_orders"]
    #print(json.dumps(oos, indent=4, sort_keys=True))