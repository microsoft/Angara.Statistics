/// <binding />
module.exports = function (grunt) {

    grunt.initConfig({       
        nugetrestore: {
            // all: {
            //     src: ['../Angara.Statistics/packages.config'],
            //     dest: ['../packages']
            // }
        },
        nugetPackFS: {
            "statistics": "../Angara.Statistics/Angara.Statistics.fsproj"
        },
        nugetpush: {
            all: {
               src: "packages/*.nupkg" 
            }
        }
    });

    grunt.loadNpmTasks('grunt-nuget');
        
    // Bug in current grunt-nuget doesn't allow to build .fsproj
    grunt.registerMultiTask('nugetPackFS', 'Create nuget package from .fsproj', function() {
       grunt.log.writeln("Creating package from " + this.data); 
       var executable = "node_modules/grunt-nuget/libs/nuget.exe"; // I assume that grunt-nuget 0.1.5 is installed
       var done = this.async();
       //invoke nuget.exe
       grunt.util.spawn({
           cmd: executable,
           args: [
               //specify the .nuspec file
               "pack",
               this.data,
 
               //specify where we want the package to be created
               "-OutputDirectory",
               "packages",
 
               "-Build",
               "-IncludeReferencedProjects"
           ] 
        }, function (error, result) {
            //output either result text or error message...
            if (error) {
                grunt.log.error(error);
            } else {
                grunt.log.write(result);
            }
            done();
        });
    });
    grunt.registerTask('default', ['nugetPackFS','nugetpush']);
};