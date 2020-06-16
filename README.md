# Lunaris

Lunaris is an app to extract and aggregate data from multiple files, with particular focus on location-sorted block-gzipped tabix-indexed files, which are typically too large to fit into memory. A typical use-case is extracting data from files stored in a Terra workspace, but any files on local disk or in Google Cloud Storage can be used.

Since files are too large to fit into memory, Lunaris relies on stream-processing, traversing all location-sorted files simultaneously in parallel, aggregating data from different files, but pointing at the same genomic location in each file at any given time. Lunaris also accepts unsorted support files as long as they are small enough to be comfortably loaded into memory.

## API Overview

Lunaris accepts a JSON file as input. Output file(s) can be specified to be TSV or JSON.

The input file contains a JSON object with properties id, regions and recipe.

The *id* field is an arbitrary String chosen by the requester to identify the request.

The *regions* field contains an object with a key for every chromosome or sequence, each pointing to an array of regions which have a start and an end.

The *recipe* field points to an object whose properties represent the steps necessary to produce the desired output.

### Example: Minimal request with TSV output

*Status: working*

A minimal request: read data from one block-gzipped, location-sorted, tabix-indexed file and export the data as tab-separated values (TSV). It contains two steps, *read* and *write*. Each step specifies a tool to describe what is being done in this step and some arguments. 

The *read* step uses the tool *IndexedRecordReader* to read from a location-sorted block-gzipped tabix-index file into a stream of objects. Required arguments are the file and the name of the id column. If the index file is omitted, it is assumed to be the data file plus suffix ".tbi".

The *write* step uses the *TSVWriter* tool to write a stream of objects to a TSV file. The from argument contains the name of the step that produces the stream of objects to be written. file is the file to be written to.

A file in Lunaris can be a local file or a file stored on Google Cloud Storage.

#### Request

~~~
{
  "id" : "requestMinimalTsv",
  "regions" : {
    "1" : [
      {
        "begin" : 100000,
        "end" : 200000
      }
    ]
  },
  "recipe" : {
    "read" : {
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/variants.tsv.gz",
      "idField" : "varId",
      "tool" : "IndexedRecordReader"
    },
    "write" : {
      "from" : "read",
      "file" : "examples/responses/responseMinimalTsv.tsv",
      "tool" : "TSVWriter"
    }
  }
}
~~~

#### Sample data (abbreviated)

~~~
#varId  chromosome      position        alt     maf     amino_acids ...
1:10583:G:A     1       10583   A       0.15852650000000001  ...
~~~

#### Response (very abbreviated)

~~~
#varId    chromosome    position    alt    maf    amino_acids ...
1:103905:A:G    1    103906    G    0.066208 ...
1:106544:C:G    1    106545    G    0.1955385 ...
1:106699:A:G    1    106700    G ...
...
~~~

### Example: Minimal request with JSON output

*Status: working*

Like the previous example, but instead of writing the output file as tab-separated values (TSV), we write it in JavaScript Object Notation (JSON). 

This is achieved by using, in the *write* step, the *JSONWriter* tool (instead of the TSVWriter).

#### Request

~~~
{
  "id" : "requestMinimalJson",
  "regions" : {
    "1" : [
      {
        "begin" : 100000,
        "end" : 200000
      }
    ]
  },
  "recipe" : {
    "read" : {
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/variants.tsv.gz",
      "idField" : "varId",
      "tool" : "IndexedRecordReader"
    },
    "write" : {
      "from" : "read",
      "file" : "examples/responses/responseMinimalJson.json",
      "tool" : "JSONWriter"
    }
  }
}
~~~

#### Sample data (abbreviated)

~~~
#varId  chromosome      position        alt     maf     amino_acids ...
1:10583:G:A     1       10583   A       0.15852650000000001  ...
~~~

#### Response (very abbreviated)

~~~
{
  "1:103905:A:G" : {
    "varId" : "1:103905:A:G",
    "chromosome" : "1",
    "position" : 103906,
    "alt" : "G",
    "maf" : "0.066208",
    "amino_acids" : "",
      ...
  },
  "1:106544:C:G" :
  ...
}
~~~

### Example: More regions

*Status: working*

The previous example only extracted region 1:100000-200000.
Here is an example that extracts data from four regions: 1:100000-200000, 1:300000-400000, X:0-100000, X:400000-500000.

#### Request

~~~
{
  "id" : "requestMoreRegions",
  "regions" : {
    "1" : [
      {
        "begin" : 100000,
          "end" : 200000
      },
      {
          "begin" : 300000,
          "end" : 400000
      }
    ],
    "X" : [
      {
          "begin" : 0,
          "end" : 100000
      },
      {
          "begin" : 400000,
          "end" : 500000
      }
    ]
  },
  "recipe" : {
    "read" : {
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/variants.tsv.gz",
      "idField" : "varId",
      "tool" : "IndexedRecordReader"
    },
    "write" : {
      "from" : "read",
      "file" : "examples/responses/responseMoreRegions.json",
      "tool" : "JSONWriter"
    }
  }
}
~~~

#### Sample data

~~~
#varId  chromosome      position        alt     maf     amino_acids ...
1:10583:G:A     1       10583   A       0.15852650000000001  ...
~~~

#### Response

~~~
{
  "1:103905:A:G" : {
    "varId" : "1:103905:A:G",
    "chromosome" : "1",
    "position" : 103906,
    "alt" : "G",
    "maf" : "0.066208",
    "amino_acids" : "",
      ...
  },
  "1:106544:C:G" :
  ...
}
~~~

### Example: Typing fields

*Status: working*

In the previous examples, all data fields were turned into JSON strings except for position. Lunaris knows from the index which is the position field (or, alternatively, the start and end fields), and it knows that these fields are integers. 
To assign other types, we use the types argument of the IndexedRecordReader. Lunaris types include "String", "File", "Int", "Float", "Bool" as well as arrays and objects.

#### Request

~~~
{
  "id" : "requestTypingFields",
  "regions" : {
    "1" : [
      {
        "begin" : 100000,
          "end" : 200000
      }
    ]
  },
  "recipe" : {
    "read" : {
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/variants.tsv.gz",
      "idField" : "varId",
      "types" : {
          "maf" : "Float"
      },
      "tool" : "IndexedRecordReader"
    },
    "write" : {
      "from" : "read",
      "file" : "examples/responses/responseTypingFields.json",
      "tool" : "JSONWriter"
    }
  }
}
~~~

#### Sample data

~~~
#varId  chromosome      position        alt     maf     amino_acids ...
1:10583:G:A     1       10583   A       0.15852650000000001  ...
~~~

#### Response

~~~
{
  "1:175810:T:A" : {
    "varId" : "1:175810:T:A",
    "chromomsome" : "1",
    "position" : 175810,
    "alt" : "A"
    "maf" : 0.654
  },
  ...
}
~~~

### Example: Filtering by field value

*Status: working*

The following example filters records, only retaining records where the field "phenotype" 
has the String "T2D" as value.

IndexedRecordReader reads the data, RecordsFilter filters, and the TSVWriter writes it.

#### Request

~~~
{
  "id" : "requestFilterTsv",
  "regions" : {
    "1" : [
      {
        "begin" : 0,
        "end" : 1000000
      }
    ]
  },
  "recipe" : {
    "read" : {
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/associations.tsv.gz",
      "idField" : "varId",
      "tool" : "IndexedRecordReader"
    },
    "filter" : {
      "from" : "read",
      "field" : "phenotype",
      "stringValue" : "T2D",
      "tool" : "RecordsFilter"
    },
    "write" : {
      "from" : "filter",
      "file" : "responseFilterTsv.tsv",
      "tool" : "TSVWriter"
    }
  }
}
~~~

#### Data

*TODO*

#### Response

*TODO*

### Example: Picking fields

*Status: in progress*

In the previous examples, all fields were included, but it is possible to pick which fields are included using the fields argument.
However, id, chromosome, position, start and end fields are never discarded.

#### Request

~~~
{
  "id" : "exampleTyping",
  "regions" : {
    "1" : [
      {
        "start" : 100000,
        "end" : 200000
      }
    ]
  },
  "recipe" : {
    "read" : {
      "tool" : "IndexedRecordReader",
      "file" : "gs://yeoldegooglebucket/variants.tsv.gz",
      "idField" : "varId"
      "fields" ["maf", "p_value"]
      "types" : {
        "maf" : "Float",
        "p_value" : "Float"
      }
    }, 
    "write" : {
      "tool" : "JSONWriter",
      "from" : "read",
      "file" : "sweetLocalFile.json"
    }
}
~~~

#### Sample data

~~~
#varId    chrom    pos    maf    p_value    is_coding
1:175810:T:A    1    175810    0.654    0.0034    false
~~~

#### Response

~~~
{
  "1:175810:T:A" : {
    "varId" : "1:175810:T:A",
    "chrom" : "1",
    "pos" : 175810,
    "maf" : 0.654,
    "p_value" : 0.0034
  },
  ...
}
~~~

### Example: Joining objects

*Status: in progress*

Objects from multiple streams can be joined into a single object to obtain a new stream of joined objects. This, however only works under the following conditions:
Each stream contains no more than one object for any given id.
Objects with the same ids must also have the same chromosome and position (or start and end), across all streams to be joined
For every id, all objects of the same id are combined into a new object with the same id and all the fields of the original object.

#### Request

~~~
{
  "id" : "exampleTyping",
  "regions" : {
    "1" : [
      {
        "start" : 100000,
        "end" : 200000
      }
    ]
  },
  "recipe" : {
    "readVariants" : {
      "tool" : "IndexedRecordReader",
      "file" : "gs://yeoldegooglebucket/variants.tsv.gz",
      "idField" : "varId"
      "types" : {
        "maf" : "Float",
        "is_coding" : "Bool"
      }
    }, 
    "readAssociations" : {
      "tool" : "IndexedRecordReader",
      "file" : "gs://yeoldegooglebucket/associations.tsv.gz",
      "idField" : "varId"
      "types" : {
        "p_value" : "Float"
      }
    }, 
    "join" : {
      "tool" : "ObjectJoiner",
      "from" : [ "readVariants", "readAssociations" ]
    }
    "write" : {
      "tool" : "JSONWriter",
      "from" : "read",
      "file" : "sweetLocalFile.json"
    }
}
~~~

#### Sample data

##### variants.tsv

~~~
#varId    chrom    pos    maf    is_coding
1:175810:T:A    1    175810    0.0034    false
~~~

##### associations.tsv

~~~
#varId    chrom    pos    p_value
1:175810:T:A    1    175810    0.0034
~~~

#### Response

~~~
{
  "1:175810:T:A" : {
    "varId" : "1:175810:T:A",
    "chrom" : "1",
    "pos" : 175810,
    "maf" : 0.654,
    "p_value" : 0.0034,
    "is_coding" : false
  },
  ...
}
~~~

*Todo: More examples*

### Example: Portal data

*Status: in progress*

A realistic example of how T2D portal data would be extracted.

#### Request

~~~
{
  "id" : "examplePortal",
  "regions" : {
    "1" : [
      {
        "start" : 100000,
        "end" : 200000
      }
    ]
  },
  "recipe" : {
    "readVariants" : {
      "tool" : "IndexedRecordReader",
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/variants.tsv.gz",
      "idField" : "varId",
      "fields" : ["varId", "chromosome", "position", "alt", "maf"],
      "types" : {
        "varId" : "String",
        "chromosome" : "String",
        "position" : Int,
        "maf" : "Float"
      }
    },
    "readAssociations" : {
      "tool" : "IndexedRecordReader",
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/associations.tsv.gz",
      "idFields" : ["varId", "phenotype"],
      "fields" : 
        [
          "varId", "chromosome", "position", "phenotype", "pValue", "beta",
          "stdErr", "zScore", "n"
        ],
      "types" : {
        "varId" : "String",
        "chromosome" : "String",
        "position" : "Int",
        "phenotype" : "String",
        "pValue" : "Float",
        "beta" : "Float",
        "stdErr" : "Float",
        "zScore" : "Float",
        "n" : "Int"
      }
    },
    "groupAssociations": {
      "tool" : "GroupAsObject",
      "from" : "readAssociations"
      "groupedFields: ["pValue", "beta", "stdErr", "zScore", "n"],
      "path" : "phenotypes"
    }
    "readPosteriors" : {
      "tool" : "IndexedRecordReader",
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/posteriors.tsv.gz",
      "idFields" : ["varId", "phenotype"],
      "fields" : 
        [
          "varId", "chromosome", "position", "phenotype", "probability",
          "credible_set_id"
        ],
      "types" : {
        "varId" : "String",
        "chromosome" : "String",
        "position" : "Int",
        "phenotype" : "String",
        "probability" : "Float",
        "credible_set_id" : "Float"
      }
    }
    "groupPosteriors": {
      "tool" : "GroupAsObject",
      "from" : "readPosteriors",
      "subIdField" : "phenotype",
      "groupedFields: ["probability", "credible_set_id"],
      "path" : "phenotypes"
    }
    "readRegions" : {
      "tool" : "IndexedRecordReader",
      "file" : "gs://fc-6fe31e1f-2c36-411c-bf23-60656d621184/data/t2d/regions.tsv.gz",
      "idFields" : ["varId", "regionId"],
      "fields" : [ "varId", "chromosome", "position", "regionId" ],
      "types" : {
        "varId" : "String",
        "chromosome" : "String",
        "position" : "Int",
        "regionId" : "Int"
      }
    }
    "readRegionTable" : {
      "tool" : "LoadTSVTable",
      "idField" : "regionId",
      "fields" : 
        [ 
          "regionId", "method", "annotation", "tissue", "tissue_description"
        ],
      "types" : {
        "regionId" : "Int",
        "method" : "String",
        "annotation" : "String",
        "tissue" : "String",
        "tissue_description" : "String"
      }
    },
    "joinRegions" : {
      "tool" : "JoinWithTable",
      "idField" : "regionId",
      "path" : "region",
      "from" : "readRegions",
      "table" : "readRegionTable"
    },
    "filterRegions" : {
      "tool" : "FilterByValues",
      "from" : "joinRegions",
      "path" : "region/annotation",
      "values" : ["GenePrediction"]
    }
    "groupRegions" : {
      "tool" : "GroupAsArray",
      "from" : "filterRegions",
      "field" : "region"
      "path" : "regions"
    }
    "join" : {
      "tool" : "ObjectJoiner"
      "from" : [ 
        "readVariants", "groupAssociations", "groupPosteriors", "groupRegions"
      ]
    }
    "write" : {
      "tool" : "JSONWriter",
      "from" : "join"
    }
  }
}
~~~

#### Response

~~~
{
  "1:175810:T:A" : {
    "varId" : "1:175810:T:A",
    "chromosome" : "1",
    "position" : 175810,
    "maf" : "0.654",
    "phenotypes" : {
      "T2D" : {
        "pValue" : 0.003, 
         "beta": 3.4, 
         "stdErr" : 1.2, 
         "zScore" : 1.2, 
         "n" : 1000,
         "probability" : 0.45,
         "credible_set_id" : "chr1:185921-4829384"
      }
    }
    "regions" : [
      {
        "method" : "ABC",
        "annotation" : "GenePrediction",
        "tissue" : "CL:0000127",
        "tissue_description" : "astrocyte"
      },
      {
        "method" : "ABC",
        "annotation" : "GenePrediction",
        "tissue" : "CL:0000129",
        "tissue_description" : "microglial cell"
      }
    ]
  },
  ...
}
~~~

### Object streams

Each location-sorted block-gzipped tabix-indexed file is read as a sorted stream of records.
A record in Lunaris is an ordered list of key/value pairs, where the key is a String and the value can be of one of a
number of types. Lunaris' types are very similar to those of JSON, but have a few more distinctions, for example 
Lunaris has an Int type.

Each record has as a minimum four core fields representing the id, the chromosome or sequence, begin and end. These 
can have any keys. begin and end may be the same field, in which case the actual end is the begin plus one, in line 
with Tabix specs. id and chromosome are of type String, while begin and end are of type Int.

The stream of records is sorted by genomic location, which means it is sorted by chromosome, begin and end, in this 
order. Multiple records in a stream can have the same id, but any records with the same id must also have the same
chromosome, begin and end. Some operations, such as joining streams or writing recordss to JSON, require that a 
stream has only one record per id. This can be ensured by grouping, which collapses all records with the same id
into one. It can also be achieved by filtering, depending on the data.

Typically, the id is a variant id, but it does not need to be.

### The recipe

The recipe is a JSON object with each property representing a step consisting of a tool and a set of arguments.
Some of these arguments are references to other steps, which means this step consumes the output of the other step,
typically a stream of records.

### Tools

#### IndexedRecordReader

This tool reads a stream of records from a given file, which can be a local file or an object on Google Cloud Storage.

##### Arguments

*file (required)*: The location of the local file or Google Cloud Storage object to read from.

*index (optional)*: The location of the index. If not given, the index is assumed to be the file plus the ".tbi" suffix.

*idField (required)*: the name of the column containing the record id.

*fields (optional)*: the fields to be read. If missing, read all fields. Core fields are always included, regardless
of this argument: idField is given separately, and the chromosome, begin and end are taken from the index file.

*types (optional)*: the types of the fields. The types of core fields are fixed to String for id and chromosome and
Int for begin and end, and cannot be changed. All fields not  explicitly typed are treated as String.

#### GroupAsObject

Merges all recordss with the same id in a stream into a single record. Core fields, which are assumed the same if
the id is the same, are kept intact. Other fields are moved to sub-objects according to the specified path followed
by the value of the subIdField.

##### Arguments

*from (required)*: reference to the step that provides the stream of records as input.

*subIdField (required)*: field whose values are used to point to the sub-objects containing the grouped fields

*groupedFields (required)*: Fields to be grouped.

*path*: path to the sub-object containing the sub-objects containing the grouped fields.

*TODO: document all tools*
