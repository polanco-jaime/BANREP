# !pip install html-table-parser-python3
import requests
import lxml.html as lh
import pandas as pd

def scrap_tutelas( keyword, anio=None, mes = None , Demandado =True):
  import urllib.request
  from pprint import pprint
  from html_table_parser.parser import HTMLTableParser
  import pandas as pd
  if mes is None:
    if anio is None:
      fec_in = '{}-{}-01'.format('1900', '01') 
      fec_fin=  '{}-{}-31'.format('2022', '12')
    else:
      fec_in = '{}-{}-01'.format(anio, '01') 
      fec_fin=  '{}-{}-31'.format(anio, '12')
  else:
    if anio is None:
      fec_in = '{}-{}-01'.format('1900', mes) 
      fec_fin=  '{}-{}-31'.format('2022', mes)
    else:
      fec_in = '{}-{}-01'.format(anio, mes) 
      fec_fin=  '{}-{}-31'.format(anio, mes)

  # pagina = 2
  # keyword = ('EPS', 'E.P.S')[0] 
  if Demandado == True:
    url = "https://www.corteconstitucional.gov.co/secretaria/consultat/consulta.php?campo=rad_petic&date3={}&date4={}&radi=Radicados&palabra={}&radi=radicados&todos=%25"
  else:
    url = 'https://www.corteconstitucional.gov.co/secretaria/consultat/consulta.php?campo=rad_actor&date3={}&date4={}&radi=Radicados&palabra={}&radi=radicados&todos=%25'
          
  url = url.format(fec_in, fec_fin, keyword.replace(' ', '+').upper() )
  # Opens a website and read its
  # binary contents (HTTP Response Body)
  def url_get_contents(url):
      # Opens a website and read its
      # binary contents (HTTP Response Body)
      #making request to the website
      req = urllib.request.Request(url=url)
      f = urllib.request.urlopen(req)
      #reading contents of the website
      return f.read()
  # defining the html contents of a URL.
  xhtml = url_get_contents(url).decode('latin-1')
  # Defining the HTMLTableParser object
  p = HTMLTableParser()
  # feeding the html contents in the # HTMLTableParser object
  p.feed(xhtml)
  table  = p.tables[0]
  table = pd.DataFrame(table, columns= table[0] )
  table= table.iloc[1:table.shape[0]]
  table['Corte'] = str(fec_fin)
  return table