# Introducción

En esta repository hay una conjunto de clases que permiten acceder a la API de Ecovadis, para permitir consultar los datos de sostenibilidad de un cliente.

En el menú de ámbito ZECOVADIS tenemos todas las transacciónes de configuración de la herramienta y testeo.

# Dependencias

Hay que tener instalado el paquete de [HTTP Services](https://github.com/irodrigob/ABAP_http_services) para poder realizar las conexiones.

# Configuración

En el menú de ámbito en la carpeta *Customizing->Conexiones* tenemos la transacción ZECV_T001 que nos permite configurar la conexión a los sistema de ecovadis.

Ecovadis proporciona dos tipos de servidores: *Sandbox* que sirve para realizar pruebas y *Production* como su nombre indica es el entorno real.

Para cada tipo de servidor hay que indicar la URL de conexión que suelen ser:

 * *Sandbox* -> api-sandbox.ecovadis-survey.com
 * *Production* -> api.ecovadis-survey.com
 
Hay que indicar la versión de la API, en el momento de crear este desarrollo es la "v2.1". Y finalmente se introduce el usuario y contraseña que nos haya proporcionado el Ecovadis.

# Clases

 * ZCL_
