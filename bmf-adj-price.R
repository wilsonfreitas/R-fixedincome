

library(XML)

url <- 'http://www2.bmf.com.br/pages/portal/bmfbovespa/boletim1/Ajustes1.asp'
tab <- readHTMLTable(url, header=TRUE, which=7)
head(tab)
tab$Mercadoria <- sub(' - .*$', '', tab$Mercadoria)
head(tab)
header <- c('Mercadoria', 'Vencimento', 'Ajuste.Ant', 'Ajuste', 'Variacao', 'Ajuste.Contr')
colnames(tab) <- header
head(tab)
to.numeric <- function (x) {
	x <- gsub('\\.', '', x)
	x <- sub(',', '.', x)
	as.numeric(x)
}

tab <- transform(tab,
	Mercadoria=ifelse(Mercadoria == '', NA, Mercadoria),
	Ajuste.Ant=to.numeric(Ajuste.Ant),
	Ajuste=to.numeric(Ajuste),
	Variacao=to.numeric(Variacao),
	Ajuste.Contr=to.numeric(Ajuste.Contr))

for (i in seq_along(tab$Mercadoria)) {
	if (is.na(tab$Mercadoria[i])) {
		tab$Mercadoria[i] <- Mercadoria.ID
	} else {
		Mercadoria.ID <- tab$Mercadoria[i]
	}
}