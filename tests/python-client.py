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
    
def get_positions_with_profits(account_id):
    answer = urllib.request.urlopen("http://127.0.0.1:8080/positions?profit_and_loss=true").read().decode()
    return json.loads(answer)

def place_order(side, symbol, quantity, at_price_type):
    url = 'http://127.0.0.1:8080/place_order?side={}&symbol={}&quantity={}&at_price_type={}'.format(side, symbol, quantity, at_price_type)
    answer = urllib.request.urlopen(url).read().decode()
    return json.loads(answer)

def commission(side, symbol, quantity, at_price_type):
    url = 'http://127.0.0.1:8080/commission?side={}&symbol={}&quantity={}&at_price_type={}'.format(side, symbol, quantity, at_price_type)
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
        commission_price = commission("BUY", symbol, quantity, "MKT")["commission"]
        total_cost = quantity * latest_price + commission_price
        if total_cost <= amount:
            found_quantity = True
        else:
            quantity = quantity - 1

    return quantity

def cancel_order(request_number):
    url = 'http://127.0.0.1:8080/cancel_order?request_number={}'.format(request_number)
    answer = urllib.request.urlopen(url).read().decode()
    return json.loads(answer)

def open_orders():
    answer = urllib.request.urlopen("http://127.0.0.1:8080/open_orders").read().decode()
    return json.loads(answer)


def test_main():
    accounts_summary = accounts_summary("NET_LIQUIDATION")
    print(json.dumps(accounts_summary, indent=4, sort_keys=True))

    main_account_id = list(accounts_summary["data"]["accounts"].keys())[0]
    print(main_account_id)

    positions_with_profits = get_positions_with_profits(main_account_id)
    positions_with_profits = positions_with_profits["data"]["accounts"][main_account_id]["positions"]
    total_profit = 0
    total_stake = 0
    for pos in positions_with_profits:
        total_stake = total_stake + positions_with_profits[pos]["open_value"]
        total_profit = total_profit + positions_with_profits[pos]["unrealized_profit"]
    
    positions_with_profits.update({"total_profit" : total_profit,
                                   "total_stake" : total_stake,
                                   "appreciation" : total_profit/total_stake})

    print(json.dumps(positions_with_profits, indent=4, sort_keys=True))

    #order = place_order ("buy", "VZIO", 44, "midprice")
    #print(json.dumps(order, indent=4, sort_keys=True))

    oos = open_orders()
    oos = oos["data"]["accounts"][main_account_id]["open_orders"]
    print(json.dumps(oos, indent=4, sort_keys=True))

    #for oo in oos:
    #    request_number = oo["request_number"]
    #    result = cancel_order(request_number)
    #    print(result)

    #oos = open_orders()
    #oos = oos["accounts"][main_account_id]["open_orders"]
    #print(json.dumps(oos, indent=4, sort_keys=True))

def test_commission():
    result = commission("BUY", "IBM", 10, "MIDPRICE")
    print(json.dumps(result, indent=4, sort_keys=True))

def test_place_order():
    result = place_order("BUY", "IBM", 10, "MIDPRICE")
    print(json.dumps(result, indent=4, sort_keys=True))
    return result

def test_open_orders():
    result = open_orders()
    print(json.dumps(result, indent=4, sort_keys=True))

def test_cancel_order(request_number):
    result = cancel_order(request_number)
    print(json.dumps(result, indent=4, sort_keys=True))

if __name__ == "__main__":

    # from another command prompt make sure to first execute:
    # ./rest_server run port:8080 gateway:ib_paper

    #test_main()
    #test_commission()
    #result = test_place_order()
    test_open_orders()
    #test_cancel_order(11915)
    
    
