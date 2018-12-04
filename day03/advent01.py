from collections import namedtuple, defaultdict, Counter

claim = namedtuple('Claim', ['ID', 'x', 'y', 'dx', 'dy'])
fabric = defaultdict(list)

def parse_file(filepath):
    with open(filepath) as f:
        for line in f:
            split = line.split()
            ID = int(split[0][1:])
            x,y= [int(temp) for temp in split[2][:-1].split(',')]
            dx,dy = [int(temp) for temp in split[3].split('x')]
            yield claim(ID, x, y, dx, dy)

def register(fabric, claim):
    for ii in range(claim.x+1, claim.x+1+claim.dx):
        for jj in range(claim.y+1, claim.y+1+claim.dy):
            fabric[(ii, jj)].append(claim.ID)
    return fabric

def get_areas(claims):
    areas = {}
    for c in claims:
        areas[c.ID] = c.dx * c.dy
    return areas

if __name__ == "__main__":
    t = parse_file('data03.txt')
    for temp in t:
        fabric = register(fabric, temp)
    
    multiple_claims = len([r for r in fabric.values() if len(r)>1])
    print(f'number of pieces claimed multiple times: {multiple_claims}')

    areas = get_areas(parse_file('data03.txt'))

    single_claim = Counter([ID[0] for ID in fabric.values() if len(ID)==1])
    for ID, area in areas.items():
        if (ID in single_claim) and (single_claim[ID] == area):
            print(f'claim that does not overlap with other claims: {ID}')