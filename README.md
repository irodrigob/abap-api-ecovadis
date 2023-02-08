# Introducción

En esta repository hay una conjunto de clases que permiten acceder a la API de Ecovadis, para permitir consultar los datos de sostenibilidad de un cliente.

En el menú de ámbito ZECOVADIS tenemos todas las transacciónes de configuración de la herramienta y testeo.

# Dependencias

Hay que tener instalado el paquete de [HTTP Services](https://github.com/irodrigob/ABAP_http_services) para poder realizar las conexiones.

# Configuración

## Conexiones

En el menú de ámbito en la carpeta *Customizing->Conexiones* tenemos la transacción ZECV_T001 que nos permite configurar la conexión a los sistema de ecovadis.

Ecovadis proporciona dos tipos de servidores: *Sandbox* que sirve para realizar pruebas y *Production* como su nombre indica es el entorno real.

Para cada tipo de servidor hay que indicar la URL de conexión que suelen ser:

 * *Sandbox* -> api-sandbox.ecovadis-survey.com
 * *Production* -> api.ecovadis-survey.com
 
Hay que indicar la versión de la API, en el momento de crear este desarrollo es la "v2.1". Y finalmente se introduce el usuario y contraseña que nos haya proporcionado el Ecovadis.

## Campos downstream

El downstream es como Ecovadis llama a los datos en la API de consulta. En la carpeta *Customizing* esta la transacción ZECV_T002 donde están configurados todos los campos del DOWNSTREAM y se puede indicar que campos se usarán para actualizarlo en los modelos propios (ejemplo en el maestro de proveedor). 

Con el método CREATE_COMPONENT_TABLE de la clase ZCL_ECV_MASTERDATA_INTEGRATION se puede crear una tabla interna dinámica con los campos configurados en la tabla anterior que tengan la columna *Save* marcada.

# Clases

 * ZCL_ECV_DATA -> Contiene la definición de tipos y constantes
 * ZCL_ECV_CONNECTION -> Encargada de la conexión con Ecovadis, es decir, obtiene el token de autentificación
 * ZCL_ECV_API_DOWNSTREAM -> Clase encargada de realizar las consultas en Ecovadis. Los métodos principales son:
   * GET_DATA_FROM_INTEGRATIONS_ID -> Obtiene los datos en base a los integrations ID. En el caso del cliente donde se implementa este desarrollo el integrations ID es el número de proveedor.
   * GET_DATA_FROM_ECOVADIS_IDS -> Obtiene los datos en base al ID de Ecovadis del proveedor.
 * ZCL_ECV_MASTERDATA_INTEGRATION ->  Es una clase abstracta que permite simplificar los procesos de lectura de datos y actualización en los modelos propios.

# Programa 

 * ZECV_TEST_CONNECTION -> Testea la conexión con Ecovadis en base a los datos de configuración
 * ZECV_GET_INTEGRATIONS_ID -> Prueba la API de consulta. Se puede consultar por Ecovadis ID o Integration ID.
