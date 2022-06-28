import tabula

def run():
	'''Transformar PDF a csv'''

	tabula.convert_into("data/claves_entidades.pdf",
		"data/claves_entidades.csv",
		output_format="csv", pages="all")


if __name__ == '__main__':
	run()