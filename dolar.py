
import requests

url = 'https://www3.bcb.gov.br/ptax_internet/consultaBoletim.do?method=consultarBoletim'
payload = dict(RadOpcao='1', ChkMoeda='61',
	DATAINI='25/04/2013', DATAFIM='25/10/2013')
r = requests.post(url, data=payload)

print r.text

# with open('SELIC.txt', 'w') as fd:
# 	for chunk in r.iter_lines():
# 		fd.write(chunk)
# 		fd.write('\n')

# f = open('SELIC.txt', 'w')
# f.write(content)
# f.close()
