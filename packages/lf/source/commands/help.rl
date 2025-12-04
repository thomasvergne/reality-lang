import core.source.color;

fn help_command(cwd: String, args: List<CLI>) -> unit {
    print(color(Green) + "LiFe Project Manager" + reset_code());
    print("Usage: lf [command] [options]");

    print("Commands:");

    print("  build       " 
        + color(Black) 
        + "Build the Reality project in the current directory." 
        + reset_code()
    );
    print("  help        " 
        + color(Black) 
        + "Show this help message." 
        + reset_code()
    );

    print("Options:");

    print("  --dev       " 
        + color(Black)
        + "Build in development mode (includes debug info)." 
        + reset_code()
    );
    print("  --release   " 
        + color(Black) 
        + "Build in release mode (optimized)." 
        + reset_code()
    );

    unit
}
