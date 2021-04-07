import urllib.request
import json

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

if __name__ == "__main__":
    account_id = "DU3689331" # replace with your account id
    
    positions = get_positions(account_id)
    print(json.dumps(positions, indent=4, sort_keys=True))

    positions_with_pnl = get_positions_with_pnls(account_id)
    print(json.dumps(positions, indent=4, sort_keys=True))

    order = place_order ("buy", "TSLA", 10, "midprice")
    print(json.dumps(order, indent=4, sort_keys=True))