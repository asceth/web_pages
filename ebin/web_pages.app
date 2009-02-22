%% This is the application resource file (.app file) for the web_pages,
%% application.
{application, web_pages,
  [{description, "Pages application to be used with web_router"},
   {vsn, "0.1.0"},
   {modules, [web_pages_app,
              web_pages_sup,
              web_pages]},
   {registered, [web_pages_sup]},
   {applications, [kernel, stdlib, web_router]},
   {mod, {web_pages_app, []}},
   {start_phases, []}]}.

