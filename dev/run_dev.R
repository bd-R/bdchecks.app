# Set options here
 # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment

# rm(list=ls(all.names = TRUE))

# Document and reload your package


# Run the application


options(golem.app.prod = FALSE); golem::detach_all_attached(); golem::document_and_reload(); bdchecks.app::bdchecks_app()
