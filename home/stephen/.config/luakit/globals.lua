globals = {
}

search_engines = {
    google      = "https://google.com/search?q=%s"
}
search_engines.default = search_engines.google

domain_props = {
    ["all"] = {
        user_stylesheet_uri     = "file://" .. luakit.data_dir .. "/styles/custom.css"
    }
}