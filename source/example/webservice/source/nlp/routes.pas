{
This file is part of the SimpleAI package.
(c) Luri Darmawan <luri@carik.id>

For the full copyright and license information, please view the LICENSE
file that was distributed with this source code.
}
unit routes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fastplaz_handler;

implementation

uses info_controller, main;

initialization
  Route.Add( 'main', TMainModule);
  Route.Add( 'info', TInfoModule);

end.

