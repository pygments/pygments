/**
 * Example Java module descriptor
 * Tests the Java Platform Module System (JPMS) syntax introduced in Java 9
 */
module com.example.myapp {
    // Required modules
    requires java.base;
    requires java.sql;
    requires transitive java.logging;
    requires static java.compiler;
    
    // Exports
    exports com.example.myapp.api;
    exports com.example.myapp.utils to com.example.client, com.example.server;
    
    // Opens for reflection
    opens com.example.myapp.internal;
    opens com.example.myapp.model to hibernate.core, com.fasterxml.jackson.databind;
    
    // Services
    uses com.example.myapp.spi.PluginService;
    provides com.example.myapp.spi.PluginService 
        with com.example.myapp.impl.DefaultPluginService,
             com.example.myapp.impl.ExtendedPluginService;
}

// Open module example
open module com.example.testapp {
    requires junit;
    exports com.example.testapp;
}
