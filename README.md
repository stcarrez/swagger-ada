# OpenAPI Ada Library

[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/openapi.json)](https://alire.ada.dev/crates/openapi)
[![Alire](https://img.shields.io/endpoint?url=https://alire.ada.dev/badges/openapi_server.json)](https://alire.ada.dev/crates/openapi_server)
[![Build Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/openapi-ada/badges/build.json)](https://porion.vacs.fr/porion/projects/view/openapi-ada/summary)
[![Test Status](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/openapi-ada/badges/tests.json)](https://porion.vacs.fr/porion/projects/view/openapi-ada/xunits)
[![Coverage](https://img.shields.io/endpoint?url=https://porion.vacs.fr/porion/api/v1/projects/openapi-ada/badges/coverage.json)](https://porion.vacs.fr/porion/projects/view/openapi-ada/summary)
[![License](https://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
[![GitLab](https://img.shields.io/badge/repo-GitLab-6C488A.svg)](https://gitlab.com/stcarrez/openapi-ada)
[![Commits](https://img.shields.io/github/commits-since/stcarrez/swagger-ada/0.6.0.svg)](Commits)

[OpenAPI Generator](https://gitlab.com/OpenAPITools/openapi-generator) is a code generator that supports generation of
API client libraries, server stubs and documentation automatically
given an [OpenAPI Spec](https://gitlab.com/OAI/OpenAPI-Specification).

The Ada client support has been integrated in [OpenAPI Generator](https://gitlab.com/OpenAPITools/openapi-generator).

The OpenAPI Ada library is a small support library for the Ada code generator
provided by OpenAPI Generator.  The library provides support to serialize the data,
make HTTP requests and support the [OpenAPI Spec](https://gitlab.com/OAI/OpenAPI-Specification).
specific operations or types.

## Version 0.7.0   - Aug 2023
  - Add support for OpenAPI Number and array of Number
  - Add support for Bearer API Key authorization
  - Update the openapi generator to version 7.0.1
    - The number type does not work consistently (float and double are not mapped correctly),
    - The consumes and produces do not allow to really use several mime types for a request or response,
    - The file and binary types are using an incorrect Ada mapping that is not supported by the Ada library,
    - Added x-ada-no-vector, x-ada-serialize-op extensions to better control the generated Ada code

[List all versions](https://gitlab.com/stcarrez/openapi-ada/blob/master/NEWS.md)

## Build with Alire

```
alr with openapi
alr with openapi_server
```

## Build and installation

The OpenAPI Ada library provides support for client and server.  The client part has
less constraints than the server part which needs more components.  For both parts,
before building this library, you may need to install the following projects:

* Ada Util      (https://gitlab.com/stcarrez/ada-util)
* Ada Security  (https://gitlab.com/stcarrez/ada-security)
* AWS      (https://libre.adacore.com/libre/tools/aws/)
* XMLAda   (https://libre.adacore.com/libre/tools/xmlada/)

If you also need to server part, you must also install the following components:

* Ada EL        (https://gitlab.com/stcarrez/ada-el)
* Ada Servlet   (https://gitlab.com/stcarrez/ada-servlet)

Then, to build OpenAPI Ada library, configure as follows:
```
./configure
make
```

And if you want the server part, configure and build with:
```
./configure --enable-server
make
```

For the installation, use the following command:
```
make install
```

The git repository comes with a pre-compiled [OpenAPI Generator](https://gitlab.com/OpenAPITools/openapi-generator)
that will be installed in `/usr/local/share/openapi-ada/openapi-generator-cli.jar`.  To help in launching the
generator, a script is installed in `/usr/local/bin/openapi-generator`.  You must have a Java JRE installed
to be able to run the generator.

## Docker

A docker container is available for those who want to try OpenAPI Ada without installing
and building all required packages.  To use the OpenAPI Ada docker container you can
run the following commands:

```
sudo docker pull ciceron/openapi-ada
```

## Using OpenAPI Ada


### Generating the REST client from OpenAPI Spec

The command to generate an Ada REST client is the following:
```
  openapi-generator generate --generator-name ada -i my-api.yaml \
       --additional-properties projectName=MyProject \
       --additional-properties openApiName=OpenAPI \
       --additional-properties httpSupport=Curl \
       --model-package MyProject.MyModule -o .
```

where *my-api.yaml* is the OpenAPI specification file that describes your API,
*MyProject* is the name of the GNAT project to use,
*MyProject.MyModule* is the name of the Ada package that will be the parent
package of the generated Ada packages.

The generator will create several files in *client/src/client* with basically
two packages: *MyProject.MyModule*.Models and *MyProject.MyModule*.Clients.
The **Models** child package will contain the type definitions for the
API operations describes in the YAML file and the **Clients** child package
will contain the *Client_Type* tagged record with the API operations to
invoke the REST API.

For example, if the YAML description file contains the following API
operation:
```
openapi: 3.0.0
servers:
  - url: 'https://todo.vacs.fr/v1'
  - url: 'http://todo.vacs.fr/v1'
info:
  title: Todo API
  description: Todo API
  version: 1.0.0
  termsOfService: 'https://todo.vacs.fr/terms/'
  contact:
    email: Stephane.Carrez@gmail.com
  license:
    name: Apache 2.0
    url: 'http://www.apache.org/licenses/LICENSE-2.0.html'
tags:
  - name: tasks
    description: Tasks
paths:
  /:
    get:
      tags:
        - tasks
      summary: Redirect to the UI
      description: |
        Default operation to redirect to the UI index page.
      operationId: redirectTodos
      responses:
        '302':
          description: redirect to the UI page
  /todos:
    get:
      tags:
        - tasks
      summary: List the available tasks
      description: |
        The list of tasks can be filtered by their status.
      operationId: listTodos
      parameters:
        - name: status
          in: query
          description: Filters the tasks by their status
          required: false
          schema:
            type: string
            enum:
              - done
              - waiting
              - working
              - all
      responses:
        '200':
          description: successful operation
          content:
            application/json:
              schema:
                type: array
                items:
                  $ref: '#/components/schemas/Todo'
        '400':
          description: Invalid status value
      security:
        - todo_auth:
            - 'read:todo'
security:
  - todo_auth: []
externalDocs:
  description: Find out more about Swagger
  url: 'http://swagger.io'
components:
  securitySchemes:
    todo_auth:
      type: oauth2
      flows:
        password:
          tokenUrl: 'http://localhost:8080/v1/oauth/token'
          scopes:
            'write:todo': Write a todo
            'read:todo': Read a todo
  schemas:
    Todo:
      type: object
      properties:
        id:
          type: integer
          format: int64
          description: The todo identifier
        title:
          type: string
          description: The todo title
        create_date:
          type: string
          format: date-time
          description: The todo creation date
        done_date:
          type: string
          format: date-time
          description: The todo resolution date
        status:
          type: string
          description: The todo state
          enum:
            - waiting
            - working
            - done
      required:
        - id
        - title
        - status
        - create_date
      example:
        id: 23
        title: Make the FOSDEM presentation
        description: password
        status: working
        create_date: '2017-12-24T00:00:00.000Z'
      xml:
        name: Todo
```

The generator will generate the following Ada code in the *Models* child package:
```
package Todos.Models is
   ...
   type Todo_Type is
     record
       Id : OpenAPI.Long;
       Title : OpenAPI.UString;
       Create_Date : OpenAPI.Datetime;
       Done_Date : OpenAPI.Nullable_Date;
       Status : OpenAPI.UString;
     end record;
     ...
end Todos.Models;
```

and the following code in the *Clients* child package:

```
package Todos.Clients is
   ...
   type Client_Type is new OpenAPI.Clients.Client_Type with null record;
   --  List the available tasks
   --  The list of tasks can be filtered by their status.
   procedure List_Todos
      (Client : in out Client_Type;
       Status : in OpenAPI.Nullable_UString;
       Result : out Todos.Models.Todo_Type_Vectors.Vector);

   --  Redirect to the UI
   --  Default operation to redirect to the UI index page.
   procedure Redirect_Todos
      (Client : in out Client_Type);
   ...
end Todos.Clients;
```

### Initialization

The HTTP/REST support is provided by [Ada Util](https://gitlab.com/stcarrez/ada-util)
and encapsulated by [OpenAPI Ada](https://gitlab.com/stcarrez/openapi-ada).  This support
is either based on Curl or on [AWS](https://libre.adacore.com/libre/tools/aws/).
The OpenAPI code generator uses Curl by default and this can be changed when running
the `openapi-generator` tool and changing the following option:

```
       --additional-properties httpSupport=aws
```


If you want to use Curl, the initialization should use the following:

```
   Util.Http.Clients.Curl.Register;
```

But if you want to use [AWS](https://libre.adacore.com/libre/tools/aws/), you will initialize with:

```
   Util.Http.Clients.AWS.Register;
```

Curl may be easier to start with because [AWS](https://libre.adacore.com/libre/tools/aws/)
does not support HTTP DELETE operation except in some latest version.

After the initialization is done, you will declare a client instance to access
the API operations:

```
   C : Todos.Clients.Client_Type;
```

The 'Client_Type' is the generated type that will implement the operations
described in the OpenAPI description file.

And you should initialize the server base URI you want to connect to:

```
  C.Set_Server ("http://localhost:8080/v1");
```

At this stage, you can use the generated operation.

### Calling an operation

Let's retrieve some todo list by calling the 'List_Todo' operation.
This operation needs an integer as input parameter and returns a 'Pet_Type'
object that contains all the pet information.   You will first declare
the pet instance as follows:

```
  List     : Todos.Models.Todo_Type_Vectors.Vector;
```

And then call the 'Get_Pet_By_Id' operation:

```
  C.List_Todos ((Is_Null => True, Value => <>), List);
```

At this stage, you can access the list of todos:

```
procedure Print (Todo : in Todos.Models.Todo_Type) is
begin
   Put (OpenAPI.Long'Image (Todo.Id));
   Set_Col (6);
   Put (OpenAPI.To_String (Todo.Status));
   Set_Col (15);
   Put (Ada.Calendar.Formatting.Image (Todo.Create_Date));
   Set_Col (40);
   if Todo.Done_Date.Is_Null then
      Put ("-");
   else
      Put (Ada.Calendar.Formatting.Image (Todo.Done_Date.Value));
   end if;
   Set_Col (60);
   Put (OpenAPI.To_String (Todo.Title));
   New_Line;
end Print;

  for T of List loop
     Print (T);
  end loop;
```

## Documentation

The OpenAPI Ada sources as well as a wiki documentation is provided on:

- [Tutorial](https://gitlab.com/stcarrez/openapi-ada/blob/master/docs/Tutorial.md)
- [Writing REST APIs with OpenAPI and Swagger Ada](https://www.slideshare.net/StephaneCarrez1/writing-rest-apis-with-openapi-and-swagger-ada/StephaneCarrez1/writing-rest-apis-with-openapi-and-swagger-ada)

