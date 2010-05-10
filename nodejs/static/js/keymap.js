/*
 *  Guacamole - Pure JavaScript/HTML VNC Client
 *  Copyright (C) 2010  Michael Jumper
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Affero General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *
 *  You should have received a copy of the GNU Affero General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

var KeyMapper = (function() {
    var unshiftedKeySym = new Array();
    unshiftedKeySym[8]   = 0xFF08; // backspace
    unshiftedKeySym[9]   = 0xFF09; // tab
    unshiftedKeySym[13]  = 0xFF0D; // enter
    unshiftedKeySym[16]  = 0xFFE1; // shift
    unshiftedKeySym[17]  = 0xFFE3; // ctrl
    unshiftedKeySym[18]  = 0xFFE9; // alt
    unshiftedKeySym[19]  = 0xFF13; // pause/break
    unshiftedKeySym[20]  = 0xFFE5; // caps lock
    unshiftedKeySym[27]  = 0xFF1B; // escape
    unshiftedKeySym[32]  = 0x20; // space
    unshiftedKeySym[33]  = 0xFF55; // page up
    unshiftedKeySym[34]  = 0xFF56; // page down
    unshiftedKeySym[35]  = 0xFF57; // end
    unshiftedKeySym[36]  = 0xFF50; // home
    unshiftedKeySym[37]  = 0xFF51; // left arrow
    unshiftedKeySym[38]  = 0xFF52; // up arrow
    unshiftedKeySym[39]  = 0xFF53; // right arrow
    unshiftedKeySym[40]  = 0xFF54; // down arrow
    unshiftedKeySym[45]  = 0xFF63; // insert
    unshiftedKeySym[46]  = 0xFFFF; // delete
    unshiftedKeySym[48]  = 0x30; // '0'
    unshiftedKeySym[49]  = 0x31; // '1'
    unshiftedKeySym[50]  = 0x32; // '2'
    unshiftedKeySym[51]  = 0x33; // '3'
    unshiftedKeySym[52]  = 0x34; // '4'
    unshiftedKeySym[53]  = 0x35; // '5'
    unshiftedKeySym[54]  = 0x36; // '6'
    unshiftedKeySym[55]  = 0x37; // '7'
    unshiftedKeySym[56]  = 0x38; // '8'
    unshiftedKeySym[57]  = 0x39; // '9'
    unshiftedKeySym[59]  = 0x3B; // semi-colon
    unshiftedKeySym[61]  = 0x3D; // equals sign
    unshiftedKeySym[65]  = 0x61; // 'a'
    unshiftedKeySym[66]  = 0x62; // 'b'
    unshiftedKeySym[67]  = 0x63; // 'c'
    unshiftedKeySym[68]  = 0x64; // 'd'
    unshiftedKeySym[69]  = 0x65; // 'e'
    unshiftedKeySym[70]  = 0x66; // 'f'
    unshiftedKeySym[71]  = 0x67; // 'g'
    unshiftedKeySym[72]  = 0x68; // 'h'
    unshiftedKeySym[73]  = 0x69; // 'i'
    unshiftedKeySym[74]  = 0x6A; // 'j'
    unshiftedKeySym[75]  = 0x6B; // 'k'
    unshiftedKeySym[76]  = 0x6C; // 'l'
    unshiftedKeySym[77]  = 0x6D; // 'm'
    unshiftedKeySym[78]  = 0x6E; // 'n'
    unshiftedKeySym[79]  = 0x6F; // 'o'
    unshiftedKeySym[80]  = 0x70; // 'p'
    unshiftedKeySym[81]  = 0x71; // 'q'
    unshiftedKeySym[82]  = 0x72; // 'r'
    unshiftedKeySym[83]  = 0x73; // 's'
    unshiftedKeySym[84]  = 0x74; // 't'
    unshiftedKeySym[85]  = 0x75; // 'u'
    unshiftedKeySym[86]  = 0x76; // 'v'
    unshiftedKeySym[87]  = 0x77; // 'w'
    unshiftedKeySym[88]  = 0x78; // 'x'
    unshiftedKeySym[89]  = 0x79; // 'y'
    unshiftedKeySym[90]  = 0x7A; // 'z'
    unshiftedKeySym[91]  = 0xFFEB; // left window key (super_l)
    unshiftedKeySym[92]  = 0xFF67; // right window key (menu key?)
    unshiftedKeySym[93]  = null; // select key
    unshiftedKeySym[96]  = 0x30; // numpad 0
    unshiftedKeySym[97]  = 0x31; // numpad 1
    unshiftedKeySym[98]  = 0x32; // numpad 2
    unshiftedKeySym[99]  = 0x33; // numpad 3
    unshiftedKeySym[100] = 0x34; // numpad 4
    unshiftedKeySym[101] = 0x35; // numpad 5
    unshiftedKeySym[102] = 0x36; // numpad 6
    unshiftedKeySym[103] = 0x37; // numpad 7
    unshiftedKeySym[104] = 0x38; // numpad 8
    unshiftedKeySym[105] = 0x39; // numpad 9
    unshiftedKeySym[106] = 0x2A; // multiply
    unshiftedKeySym[107] = 0x3D; // equals
    unshiftedKeySym[109] = 0x2D; // subtract
    unshiftedKeySym[110] = 0x2E; // decimal point
    unshiftedKeySym[111] = 0x2F; // divide
    unshiftedKeySym[112] = 0xFFBE; // f1
    unshiftedKeySym[113] = 0xFFBF; // f2
    unshiftedKeySym[114] = 0xFFC0; // f3
    unshiftedKeySym[115] = 0xFFC1; // f4
    unshiftedKeySym[116] = 0xFFC2; // f5
    unshiftedKeySym[117] = 0xFFC3; // f6
    unshiftedKeySym[118] = 0xFFC4; // f7
    unshiftedKeySym[119] = 0xFFC5; // f8
    unshiftedKeySym[120] = 0xFFC6; // f9
    unshiftedKeySym[121] = 0xFFC7; // f10
    unshiftedKeySym[122] = 0xFFC8; // f11
    unshiftedKeySym[123] = 0xFFC9; // f12
    unshiftedKeySym[144] = 0xFF7F; // num lock
    unshiftedKeySym[145] = 0xFF14; // scroll lock
    unshiftedKeySym[186] = 0x3B; // semi-colon
    unshiftedKeySym[187] = 0x3D; // equal sign
    unshiftedKeySym[188] = 0x2C; // comma
    unshiftedKeySym[189] = 0x2D; // dash
    unshiftedKeySym[190] = 0x2E; // period
    unshiftedKeySym[191] = 0x2F; // forward slash
    unshiftedKeySym[192] = 0x60; // grave accent
    unshiftedKeySym[219] = 0x5B; // open bracket
    unshiftedKeySym[220] = 0x5C; // back slash
    unshiftedKeySym[221] = 0x5D; // close bracket
    unshiftedKeySym[222] = 0x27; // single quote


    // Shifted versions, IF DIFFERENT FROM UNSHIFTED!
    // If any of these are null, the unshifted one will be used.
    var shiftedKeySym  = new Array();
    shiftedKeySym[8]   = null; // backspace
    shiftedKeySym[9]   = null; // tab
    shiftedKeySym[13]  = null; // enter
    shiftedKeySym[16]  = null; // shift
    shiftedKeySym[17]  = null; // ctrl
    shiftedKeySym[18]  = 0xFFE7; // alt
    shiftedKeySym[19]  = null; // pause/break
    shiftedKeySym[20]  = null; // caps lock
    shiftedKeySym[27]  = null; // escape
    shiftedKeySym[32]  = null; // space
    shiftedKeySym[33]  = null; // page up
    shiftedKeySym[34]  = null; // page down
    shiftedKeySym[35]  = null; // end
    shiftedKeySym[36]  = null; // home
    shiftedKeySym[37]  = null; // left arrow
    shiftedKeySym[38]  = null; // up arrow
    shiftedKeySym[39]  = null; // right arrow
    shiftedKeySym[40]  = null; // down arrow
    shiftedKeySym[45]  = null; // insert
    shiftedKeySym[46]  = null; // delete
    shiftedKeySym[48]  = 0x29; // ')'
    shiftedKeySym[49]  = 0x21; // '!'
    shiftedKeySym[50]  = 0x40; // '@'
    shiftedKeySym[51]  = 0x23; // '#'
    shiftedKeySym[52]  = 0x24; // '$'
    shiftedKeySym[53]  = 0x25; // '%'
    shiftedKeySym[54]  = 0x5E; // '^'
    shiftedKeySym[55]  = 0x26; // '&'
    shiftedKeySym[56]  = 0x2A; // '*'
    shiftedKeySym[57]  = 0x28; // '('
    shiftedKeySym[59]  = 0x3A; // colon
    shiftedKeySym[61]  = 0x2B; // plus sign
    shiftedKeySym[65]  = 0x41; // 'A'
    shiftedKeySym[66]  = 0x42; // 'B'
    shiftedKeySym[67]  = 0x43; // 'C'
    shiftedKeySym[68]  = 0x44; // 'D'
    shiftedKeySym[69]  = 0x45; // 'E'
    shiftedKeySym[70]  = 0x46; // 'F'
    shiftedKeySym[71]  = 0x47; // 'G'
    shiftedKeySym[72]  = 0x48; // 'H'
    shiftedKeySym[73]  = 0x49; // 'I'
    shiftedKeySym[74]  = 0x4A; // 'J'
    shiftedKeySym[75]  = 0x4B; // 'K'
    shiftedKeySym[76]  = 0x4C; // 'L'
    shiftedKeySym[77]  = 0x4D; // 'M'
    shiftedKeySym[78]  = 0x4E; // 'N'
    shiftedKeySym[79]  = 0x4F; // 'O'
    shiftedKeySym[80]  = 0x50; // 'P'
    shiftedKeySym[81]  = 0x51; // 'Q'
    shiftedKeySym[82]  = 0x52; // 'R'
    shiftedKeySym[83]  = 0x53; // 'S'
    shiftedKeySym[84]  = 0x54; // 'T'
    shiftedKeySym[85]  = 0x55; // 'U'
    shiftedKeySym[86]  = 0x56; // 'V'
    shiftedKeySym[87]  = 0x57; // 'W'
    shiftedKeySym[88]  = 0x58; // 'X'
    shiftedKeySym[89]  = 0x59; // 'Y'
    shiftedKeySym[90]  = 0x5A; // 'Z'
    shiftedKeySym[91]  = null; // left window key
    shiftedKeySym[92]  = null; // right window key
    shiftedKeySym[93]  = null; // select key
    shiftedKeySym[96]  = null; // numpad 0
    shiftedKeySym[97]  = null; // numpad 1
    shiftedKeySym[98]  = null; // numpad 2
    shiftedKeySym[99]  = null; // numpad 3
    shiftedKeySym[100] = null; // numpad 4
    shiftedKeySym[101] = null; // numpad 5
    shiftedKeySym[102] = null; // numpad 6
    shiftedKeySym[103] = null; // numpad 7
    shiftedKeySym[104] = null; // numpad 8
    shiftedKeySym[105] = null; // numpad 9
    shiftedKeySym[106] = null; // multiply
    shiftedKeySym[107] = 0x2B; // add
    shiftedKeySym[109] = 0x5F; // subtract
    shiftedKeySym[110] = null; // decimal point
    shiftedKeySym[111] = null; // divide
    shiftedKeySym[112] = null; // f1
    shiftedKeySym[113] = null; // f2
    shiftedKeySym[114] = null; // f3
    shiftedKeySym[115] = null; // f4
    shiftedKeySym[116] = null; // f5
    shiftedKeySym[117] = null; // f6
    shiftedKeySym[118] = null; // f7
    shiftedKeySym[119] = null; // f8
    shiftedKeySym[120] = null; // f9
    shiftedKeySym[121] = null; // f10
    shiftedKeySym[122] = null; // f11
    shiftedKeySym[123] = null; // f12
    shiftedKeySym[144] = null; // num lock
    shiftedKeySym[145] = null; // scroll lock
    shiftedKeySym[186] = 0x3A; // colon
    shiftedKeySym[187] = 0x2B; // plus sign
    shiftedKeySym[188] = 0x3C; // less than
    shiftedKeySym[189] = 0x5F; // underscore
    shiftedKeySym[190] = 0x3E; // greater than
    shiftedKeySym[191] = 0x3F; // question mark
    shiftedKeySym[192] = 0x7E; // tilde
    shiftedKeySym[219] = 0x7B; // open brace
    shiftedKeySym[220] = 0x7C; // pipe
    shiftedKeySym[221] = 0x7D; // close brace
    shiftedKeySym[222] = 0x22; // double quote

    return {
        get_key_sym: function(keycode, shift) {
            var table = shift ? shiftedKeySym : unshiftedKeySym;
            return table[keycode] || keycode;
      }
    };

})();

