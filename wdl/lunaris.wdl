version 1.0

workflow lunaris {
  input {
    File lunaris_input
    Array[File] data_files
    Array[String] output_file_names
  }
  call run_lunaris {
    input:
      lunaris_input = lunaris_input,
      data_files = data_files,
      output_file_names = output_file_names
  }
}

task run_lunaris {
  input {
    File lunaris_input
    Array[File] data_files
    Array[String] output_file_names
  }
  parameter_meta {
    lunaris_input: {
      localization_optional: true
    }
    data_files: {
      localization_optional: true
    }
  }
  runtime {
    preemptible: 3
    docker: "gcr.io/v2f-public-resources/lunaris:1.1.0"
    cpu: 1
    memory: "5 GB"
    disks: "local-disk 20 HDD"
  }
  command <<<
    lunaris batch --request-file=~{lunaris_input}
  >>>
  output {
    Array[File] output_files = output_file_names
  }
}