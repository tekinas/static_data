project('static_data','cpp', default_options : ['warning_level=3'])

static_data = declare_dependency(include_directories : 'include/')
fmt = dependency('fmt', method : 'cmake', modules : ['fmt::fmt-header-only'])

executable('static_data_test', ['test/static_data_test.cpp'] , dependencies : [fmt, static_data])
executable('static_type_test', ['test/static_type_test.cpp'] , dependencies : [fmt, static_data])
executable('static_json', ['test/static_json.cpp'] , dependencies : [fmt, static_data])
