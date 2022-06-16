# APP_EDM
Dashboard App for a class project


link al dahboard: https://edm-dashboard-jpgcd.shinyapps.io/DASHBOARD-EDM-JP/

## 1. Trabajo realizado

En este proyecto hemos seguido un esquema estructurado para realizar el proyecto. Hemos comenzado con un boceto de lo que pretendíamos hacer, después hemos realizado el código para implementarlo y finalmente lo hemos completado con todos los aspectos estéticos.

El boceto lo hemos realizado en Google slides, y decidimos estructurarlo en dos pestañas, una para la carga y transformación de datos y la otra para el modelo, como se muestra en la siguiente imagen: 

https://raw.githubusercontent.com/jordicr23/APP_EDM/main/boceto.png

Después hemos implementamos el código que realiza lo indicado por el boceto. Nos hemos encontrado con varios contratiempos, como por ejemplo que el modelo al implementarlo en shiny no funcionaba por ciertas características de la librería, pero finalmente implementamos todo lo que queríamos. 

Sobre todo estamos muy satisfechos con ciertos elementos que mejoran la experiencia del usuario, como una casilla para seleccionar las variables del modelo, junto con el botón para seleccionarlas todas, un asterisco rojo (*) en las casillas obligatorias o un botón para calcular los modelos una vez todas las características están seleccionadas. También otras funcionalidades sutiles pero a la vez relevantes como un spinner de carga del gráfico, que informa al usuario de que se están procesando los datos, o simplemente el hecho de que cuando no hay datos, no se permite al usuario seleccionar las variables.

Una de las principales ventajas de nuestro dashboard es que se pueden calcular varios modelos a la vez y se pueden comparar tanto las curvas ROC, como el AUC y el coste óptimo de cada uno. Decidimos usar tres modelos típicos de clasificación: Regresión logística, SVM y PLS-DA. Por supuesto podríamos haber incluido otros, pero creemos que sería ponerle demasiada carga al servidor sobre todo siendo que se trata de una demostración. 
Idea de negocio
Hoy en día cada vez más empresas basan su capacidad de alcance a los posibles clientes en la aplicación de métodos de clasificación de estos en distintos grupos. Nuestra idea de negocio es proporcionar a grandes empresas una interfaz gráfica que les permita clasificar a los clientes sin la necesidad de utilizar alguna herramienta compleja en dos grupos: los clientes que pueden recibir una campaña de marketing favorablemente y los que no. De este modo, la organización de la empresa puede decidirse a enfocar sus esfuerzos de marketing y así evitar malgastar sus recursos tanto físicos como económicos. Con nuestra herramienta informaríamos a la empresa cuál es el coste que se ahorrarían respecto a la estrategia que siguen en la actualidad, y nosotros como fundadores de esta utilidad mantendriamos la herramienta en constante evolución para satisfacer las demandas variables de nuestros clientes y cobraríamos un 25% del ahorro de la empresa por cambiar de política.

## 2. Idea de negocio

Hoy en día cada vez más empresas basan su capacidad de alcance a los posibles clientes en la aplicación de métodos de clasificación de estos en distintos grupos. Nuestra idea de negocio es proporcionar a grandes empresas una interfaz gráfica que les permita clasificar a los clientes sin la necesidad de utilizar alguna herramienta compleja en dos grupos: los clientes que pueden recibir una campaña de marketing favorablemente y los que no. De este modo, la organización de la empresa puede decidirse a enfocar sus esfuerzos de marketing y así evitar malgastar sus recursos tanto físicos como económicos. Con nuestra herramienta informaríamos a la empresa cuál es el coste que se ahorrarían respecto a la estrategia que siguen en la actualidad, y nosotros como fundadores de esta utilidad mantendriamos la herramienta en constante evolución para satisfacer las demandas variables de nuestros clientes y cobraríamos un 25% del ahorro de la empresa por cambiar de política.




