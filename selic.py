
import requests
'''
p <- POST('http://www3.bcb.gov.br/selic/consulta/taxaSelic.do?method=listarTaxaDiaria',
	body=list(tipoApresentacao='arquivo', fatorCorrecao='asc',
		method='listarTaxaDiaria', idioma='P',
		dataInicial='25/10/2003', dataFinal='25/10/2013'))
'''
payload = dict(tipoApresentacao='arquivo', fatorCorrecao='asc',
	method='listarTaxaDiaria', idioma='P',
	dataInicial='01/01/1994', dataFinal='25/10/2013')
r = requests.post('http://www3.bcb.gov.br/selic/consulta/taxaSelic.do', data=payload)

# print len(r.text)

with open('SELIC.txt', 'w') as fd:
	for chunk in r.iter_lines():
		fd.write(chunk)
		fd.write('\n')

# f = open('SELIC.txt', 'w')
# f.write(content)
# f.close()