# Swagger Ada Library

[![Build Status](https://img.shields.io/jenkins/s/http/jenkins.vacs.fr/Ada-Swagger.svg)](http://jenkins.vacs.fr/job/Ada-Swagger/)
[![Test Status](https://img.shields.io/jenkins/t/http/jenkins.vacs.fr/Ada-Swagger.svg)](http://jenkins.vacs.fr/job/Ada-Swagger/)
[![License](http://img.shields.io/badge/license-APACHE2-blue.svg)](LICENSE)
![Commits](https://img.shields.io/github/commits-since/stcarrez/swagger-awa/swagger-ada-1.0.0.svg)

[Swagger Codegen](https://github.com/swagger-api/swagger-codegen) is a code generator that supports generation of
API client libraries, server stubs and documentation automatically
given an [OpenAPI Spec](https://github.com/OAI/OpenAPI-Specification).

The Ada client support has been integrated in [Swagger Codegen](https://github.com/swagger-api/swagger-codegen).

The Swagger Ada library is a small support library for the Ada code generator
provided by Swagger Codegen.  The library provides support to serialize the data,
make HTTP requests and support the [OpenAPI Spec](https://github.com/OAI/OpenAPI-Specification).
specific operations or types.

## Build and installation
Before building this library, you may need to install the following projects:

* Ada Util      (https://github.com/stcarrez/ada-util)
* AWS      (http://libre.adacore.com/libre/tools/aws/)
* XMLAda   (http://libre.adacore.com/libre/tools/xmlada/)

Then, to build Ada Swagger library, configure as follows:
```
   ./configure
   make
```

For the installation, use the following command:
```
   make install
```

## Using Swagger Ada

### Generating the REST client from OpenAPI Spec

The command to generate an Ada REST client is the following:
```
  java -jar swagger-codegen-cli.jar generate -l ada -i my-api.yaml -o client \
       -DprojectName=MyProject --model-package MyProject.MyModule
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
paths:
  '/pet/{petId}':
    get:
      tags:
        - pet
      summary: Find pet by ID
      description: Returns a single pet
      operationId: getPetById
      produces:
        - application/xml
        - application/json
      parameters:
        - name: petId
          in: path
          description: ID of pet to return
          required: true
          type: integer
          format: int64
      responses:
        '200':
          description: successful operation
          schema:
            $ref: '#/definitions/Pet'
        '400':
          description: Invalid ID supplied
        '404':
          description: Pet not found
      security:
        - api_key: []
definitions:
  Pet:
    title: a Pet
    description: A pet for sale in the pet store
    type: object
    required:
      - name
      - photoUrls
    properties:
      id:
        type: integer
        format: int64
      category:
        $ref: '#/definitions/Category'
      name:
        type: string
        example: doggie
      photoUrls:
        type: array
        xml:
          name: photoUrl
          wrapped: true
        items:
          type: string
      tags:
        type: array
        xml:
          name: tag
          wrapped: true
        items:
          $ref: '#/definitions/Tag'
      status:
        type: string
        description: pet status in the store
        enum:
          - available
          - pending
          - sold
    xml:
      name: Pet
```

The generator will generate the following Ada code in the *Models* child package:
```
package Samples.Petstore.Models is
   ...
   type Pet_Type is
     record
       Id : Swagger.Long;
       Category : Samples.Petstore.Models.Category_Type;
       Name : Swagger.UString;
       Photo_Urls : Swagger.UString_Vectors.Vector;
       Tags : Samples.Petstore.Models.Tag_Type_Vectors.Vector;
       Status : Swagger.UString;
     end record;
     ...
end Samples.Petstore.Models;
```

and the following code in the *Clients* child package:

```
package Samples.Petstore.Clients is
   ...
   type Client_Type is new Swagger.Clients.Client_Type with null record;
   procedure Get_Pet_By_Id
      (Client : in out Client_Type;
       Pet_Id : in Swagger.Long;
       Result : out Samples.Petstore.Models.Pet_Type);
   ...
end Samples.Petstore.Clients;
```

### Initialization

The HTTP/REST support is provided by [Ada Util](https://github.com/stcarrez/ada-util)
and encapsulated by [Swagger Ada](https://github.com/stcarrez/swagger-ada).  If you want
to use Curl, you should initialize with the following:

```
   Util.Http.Clients.Curl.Register;
```

But if you want to use [AWS](http://libre.adacore.com/libre/tools/aws/), you will initialize with:

```
   Util.Http.Clients.Web.Register;
```

Curl may be easier to start with because [AWS](http://libre.adacore.com/libre/tools/aws/)
does not support HTTP DELETE operation except in some latest version.

After the initialization is done, you will declare a client instance to access
the API operations:

```
   C : Samples.Petstore.Clients.Client_Type;
```

The 'Client_Type' is the generated type that will implement the operations
described in the OpenAPI description file.

And you should initialize the server base URI you want to connect to:

```
  C.Set_Server ("http://petstore.swagger.io/v2");
```

At this stage, you can use the generated operation.

### Calling an operation

Let's retrieve some pet information by calling the 'Get_Pet_By_Id' operation.
This operation needs an integer as input parameter and returns a 'Pet_Type'
object that contains all the pet information.   You will first declare
the pet instance as follows:

```
  Pet  : Samples.Petstore.Models.Pet_Type;
```

And then call the 'Get_Pet_By_Id' operation:

```
  C.Get_Pet_By_Id (768, Pet);
```

At this stage, you can access information from the 'Pet' instance:

```
  Ada.Text_IO.Put_Line ("Id      : " & Swagger.Long'Image (Pet.Id));
  Ada.Text_IO.Put_Line ("Name    : " & Swagger.To_String (Pet.Name));
  Ada.Text_IO.Put_Line ("Status  : " & Swagger.To_String (Pet.Status));
```

## Documentation

The Ada Swagger sources as well as a wiki documentation is provided on:

   https://github.com/stcarrez/swagger-ada/wiki

