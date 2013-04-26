CMAKE_MINIMUM_REQUIRED(VERSION 2.8.1 FATAL_ERROR)

SET(ANYDSLEXP_REQUIRED_PROJECTS 
    anydsl2
)

SET(ANYDSLEXP_RECOMMENDED_PROJECTS
    impala
    app
)

SET(ANYDSLEXP_SKIP_DIRS 
    .
    ..
    build
)

INCLUDE_DIRECTORIES(
    ${PROJ_SOURCE_DIR}
)

LINK_DIRECTORIES(
    ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
)

add_subdirectories_with_option(
    "ANYDSLEXP_BUILD"
    REQUIRED ${ANYDSLEXP_REQUIRED_PROJECTS}
    RECOMMENDED ${ANYDSLEXP_RECOMMENDED_PROJECTS}
    SKIP ${ANYDSLEXP_SKIP_DIRS}
)