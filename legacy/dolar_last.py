
import requests

url = 'https://www3.bcb.gov.br/ptax_internet/consultarUltimaCotacaoDolar.do'
r = requests.get(url)

print r.text

# with open('SELIC.txt', 'w') as fd:
# 	for chunk in r.iter_lines():
# 		fd.write(chunk)
# 		fd.write('\n')

# f = open('SELIC.txt', 'w')
# f.write(content)
# f.close()
