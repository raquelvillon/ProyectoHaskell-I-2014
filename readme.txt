Descripción del Proyecto de Haskell
Van a implementar un motor de búsquedas de vuelos. Si no han usado uno antes, su proyecto resultado será una simplificación de este: http://matrix.itasoftware.com
La idea de un motor es proveer al usuario información acerca de rutas de vuelo que le permitan llegar de un lugar a otro con el menor costo posible (monetario o de tiempo, pasando por X o Y ciudades). En los vuelos internacionales lo más común es que haya más de una ruta y que el viajero deba de hacer escalas en su ruta.
Los componentes del proyecto incluyen:
1) Conseguir los datos. Deben de conseguir de internet o algún contacto suyo un listado de vuelos con precios/distancias/tiempo. En internet hay estos recursos pero hay que saber encontrarlos. Lo más seguro es que les toque pardear esta información para ponerla en un grafo. Les ofrezco la posibilidad de parsear la información que consigan en Perl, en lugar de Haskell. Con la experiencia que tienen ahora se les debería hacer mucho más fácil. Es MUY IMPORTANTE que citen sus fuentes de la información. No debe de ser inventada ni formateada a mano. Lo que consigan pueden parsearlo con perl o haskell y demostrar que la están utilizando en su estado original. Los datos no pueden aparecer mágicamente cargados en su programa. Deberían de haber como poco unos 20 destinos.
2) La interfaz del programa puede ser tan fácil (preguntas por la línea de comandos o argumentos de la llamada del programa) o tan compleja (GUI o web) como ustedes deseen. Deben de proveer la opción de indicar puerto de partida y destino, opcionalmente puertos intermedios. Adicionalmente debe de haber una manera de seleccionar que factor tiene prioridad (tiempo, costo u otro).
* Mucho ojo que estoy obviando las fechas/horas de los itinerarios. Si alguien puede incluir este factor el proceso se vuelve mucho más complejo así como más realista. Sería muy interesante que implementaran algo así de complejo y sin duda consideraría una bonificación significativa en estos casos.