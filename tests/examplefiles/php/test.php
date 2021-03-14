<?php

$disapproval_ಠ_ಠ_of_php = 'unicode var';

$test = function($a) { $lambda = 1; }

/**
 *  Zip class file
 *
 *  @package     fnord.bb
 *  @subpackage  archive
 */

// Unlock?
if(!defined('UNLOCK') || !UNLOCK)
  die();
  
// Load the parent archive class
require_once(ROOT_PATH.'/classes/archive.class.php');

class Zip\Zippಠ_ಠ_ {

}

/**
 *  Zip class
 *
 *  @author      Manni <manni@fnord.name>
 *  @copyright   Copyright (c) 2006, Manni
 *  @version     1.0
 *  @link        http://www.pkware.com/business_and_developers/developer/popups/appnote.txt
 *  @link        http://mannithedark.is-a-geek.net/
 *  @since       1.0
 *  @package     fnord.bb
 *  @subpackage  archive
 */
class Zip extends Archive {
 /**
  *  Outputs the zip file
  *
  *  This function creates the zip file with the dirs and files given.
  *  If the optional parameter $file is given, the zip file is will be
  *  saved at that location. Otherwise the function returns the zip file's content.
  *
  *  @access                   public
  *
  *  @link                     http://www.pkware.com/business_and_developers/developer/popups/appnote.txt
  *  @param  string $filename  The path where the zip file will be saved
  *
  *  @return bool|string       Returns either true if the fil is sucessfully created or the content of the zip file
  */
  function out($filename = false) {
    // Empty output
    $file_data = array(); // Data of the file part
    $cd_data   = array(); // Data of the central directory

    // Sort dirs and files by path length
    uksort($this->dirs,  'sort_by_length');
    uksort($this->files, 'sort_by_length');

    // Handle dirs
    foreach($this->dirs as $dir) {
      $dir .= '/';
      // File part

      // Reset dir data
      $dir_data = '';

      // Local file header
      $dir_data .= "\x50\x4b\x03\x04";      // Local file header signature
      $dir_data .= pack("v", 10);           // Version needed to extract
      $dir_data .= pack("v", 0);            // General purpose bit flag
      $dir_data .= pack("v", 0);            // Compression method
      $dir_data .= pack("v", 0);            // Last mod file time
      $dir_data .= pack("v", 0);            // Last mod file date
      $dir_data .= pack("V", 0);            // crc-32
      $dir_data .= pack("V", 0);            // Compressed size
      $dir_data .= pack("V", 0);            // Uncompressed size
      $dir_data .= pack("v", strlen($dir)); // File name length
      $dir_data .= pack("v", 0);            // Extra field length

      $dir_data .= $dir;                    // File name
      $dir_data .= '';                      // Extra field (is empty)

      // File data
      $dir_data .= '';                      // Dirs have no file data

      // Data descriptor
      $dir_data .= pack("V", 0);            // crc-32
      $dir_data .= pack("V", 0);            // Compressed size
      $dir_data .= pack("V", 0);            // Uncompressed size

      // Save current offset
      $offset = strlen(implode('', $file_data));

      // Append dir data to the file part
      $file_data[] = $dir_data;

      // Central directory

      // Reset dir data
      $dir_data = '';

      // File header
      $dir_data .= "\x50\x4b\x01\x02";      // Local file header signature
      $dir_data .= pack("v", 0);            // Version made by
      $dir_data .= pack("v", 10);           // Version needed to extract
      $dir_data .= pack("v", 0);            // General purpose bit flag
      $dir_data .= pack("v", 0);            // Compression method
      $dir_data .= pack("v", 0);            // Last mod file time
      $dir_data .= pack("v", 0);            // Last mod file date
      $dir_data .= pack("V", 0);            // crc-32
      $dir_data .= pack("V", 0);            // Compressed size
      $dir_data .= pack("V", 0);            // Uncompressed size
      $dir_data .= pack("v", strlen($dir)); // File name length
      $dir_data .= pack("v", 0);            // Extra field length
      $dir_data .= pack("v", 0);            // File comment length
      $dir_data .= pack("v", 0);            // Disk number start
      $dir_data .= pack("v", 0);            // Internal file attributes
      $dir_data .= pack("V", 16);           // External file attributes
      $dir_data .= pack("V", $offset);      // Relative offset of local header

      $dir_data .= $dir;                    // File name
      $dir_data .= '';                      // Extra field (is empty)
      $dir_data .= '';                      // File comment (is empty)

      /*
      // Data descriptor
      $dir_data .= pack("V", 0);            // crc-32
      $dir_data .= pack("V", 0);            // Compressed size
      $dir_data .= pack("V", 0);            // Uncompressed size
      */
      
      // Append dir data to the central directory data
      $cd_data[] = $dir_data;
    }

    // Handle files
    foreach($this->files as $name => $file) {
      // Get values
      $content = $file[0];
    
      // File part

      // Reset file data
      $fd = '';
      
      // Detect possible compressions
      // Use deflate
      if(function_exists('gzdeflate')) {
        $method = 8;

        // Compress file content
        $compressed_data = gzdeflate($content);

      // Use bzip2
      } elseif(function_exists('bzcompress')) {
        $method = 12;

        // Compress file content
        $compressed_data = bzcompress($content);

      // No compression
      } else {
        $method = 0;

        // Do not compress the content :P
        $compressed_data = $content;
      }

      // Local file header
      $fd .= "\x50\x4b\x03\x04";                  // Local file header signature
      $fd .= pack("v", 20);                       // Version needed to extract
      $fd .= pack("v", 0);                        // General purpose bit flag
      $fd .= pack("v", $method);                  // Compression method
      $fd .= pack("v", 0);                        // Last mod file time
      $fd .= pack("v", 0);                        // Last mod file date
      $fd .= pack("V", crc32($content));          // crc-32
      $fd .= pack("V", strlen($compressed_data)); // Compressed size
      $fd .= pack("V", strlen($content));         // Uncompressed size
      $fd .= pack("v", strlen($name));            // File name length
      $fd .= pack("v", 0);                        // Extra field length

      $fd .= $name;                               // File name
      $fd .= '';                                  // Extra field (is empty)

      // File data
      $fd .= $compressed_data;
      
      // Data descriptor
      $fd .= pack("V", crc32($content));          // crc-32
      $fd .= pack("V", strlen($compressed_data)); // Compressed size
      $fd .= pack("V", strlen($content));         // Uncompressed size

      // Save current offset
      $offset = strlen(implode('', $file_data));

      // Append file data to the file part
      $file_data[] = $fd;

      // Central directory

      // Reset file data
      $fd = '';

      // File header
      $fd .= "\x50\x4b\x01\x02";                  // Local file header signature
      $fd .= pack("v", 0);                        // Version made by
      $fd .= pack("v", 20);                       // Version needed to extract
      $fd .= pack("v", 0);                        // General purpose bit flag
      $fd .= pack("v", $method);                  // Compression method
      $fd .= pack("v", 0);                        // Last mod file time
      $fd .= pack("v", 0);                        // Last mod file date
      $fd .= pack("V", crc32($content));          // crc-32
      $fd .= pack("V", strlen($compressed_data)); // Compressed size
      $fd .= pack("V", strlen($content));         // Uncompressed size
      $fd .= pack("v", strlen($name));            // File name length
      $fd .= pack("v", 0);                        // Extra field length
      $fd .= pack("v", 0);                        // File comment length
      $fd .= pack("v", 0);                        // Disk number start
      $fd .= pack("v", 0);                        // Internal file attributes
      $fd .= pack("V", 32);                       // External file attributes
      $fd .= pack("V", $offset);                  // Relative offset of local header

      $fd .= $name;                               // File name
      $fd .= '';                                  // Extra field (is empty)
      $fd .= '';                                  // File comment (is empty)

      /*
      // Data descriptor
      $fd .= pack("V", crc32($content));          // crc-32
      $fd .= pack("V", strlen($compressed_data)); // Compressed size
      $fd .= pack("V", strlen($content));         // Uncompressed size
      */

      // Append file data to the central directory data
      $cd_data[] = $fd;
    }

    // Digital signature
    $digital_signature = '';
    $digital_signature .= "\x50\x4b\x05\x05";  // Header signature
    $digital_signature .= pack("v", 0);        // Size of data
    $digital_signature .= '';                  // Signature data (is empty)

    $tmp_file_data = implode('', $file_data);  // File data
    $tmp_cd_data   = implode('', $cd_data).    // Central directory
                     $digital_signature;       // Digital signature

    // End of central directory
    $eof_cd = '';
    $eof_cd .= "\x50\x4b\x05\x06";                // End of central dir signature
    $eof_cd .= pack("v", 0);                      // Number of this disk
    $eof_cd .= pack("v", 0);                      // Number of the disk with the start of the central directory
    $eof_cd .= pack("v", count($cd_data));        // Total number of entries in the central directory on this disk
    $eof_cd .= pack("v", count($cd_data));        // Total number of entries in the central directory
    $eof_cd .= pack("V", strlen($tmp_cd_data));   // Size of the central directory
    $eof_cd .= pack("V", strlen($tmp_file_data)); // Offset of start of central directory with respect to the starting disk number
    $eof_cd .= pack("v", 0);                      // .ZIP file comment length
    $eof_cd .= '';                                // .ZIP file comment (is empty)

    // Content of the zip file
    $data = $tmp_file_data.
            // $extra_data_record.
            $tmp_cd_data.
            $eof_cd;

    // Return content?
    if(!$filename)
      return $data;
      
    // Write to file
    return file_put_contents($filename, $data);
  }
  
 /**
  *  Load a zip file
  *
  *  This function loads the files and dirs from a zip file from the harddrive.
  *
  *  @access                public
  *
  *  @param  string $file   The path to the zip file
  *  @param  bool   $reset  Reset the files and dirs before adding the zip file's content?
  *
  *  @return bool           Returns true if the file was loaded sucessfully
  */
  function load_file($file, $reset = true) {
    // Check whether the file exists
    if(!file_exists($file))
      return false;

    // Load the files content
    $content = @file_get_contents($file);

    // Return false if the file cannot be opened
    if(!$content)
      return false;

    // Read the zip
    return $this->load_string($content, $reset);
  }
  
 /**
  *  Load a zip string
  *
  *  This function loads the files and dirs from a string
  *
  *  @access                 public
  *
  *  @param  string $string  The string the zip is generated from
  *  @param  bool   $reset   Reset the files and dirs before adding the zip file's content?
  *
  *  @return bool            Returns true if the string was loaded sucessfully
  */
  function load_string($string, $reset = true) {
    // Reset the zip?
    if($reset) {
      $this->dirs  = array();
      $this->files = array();
    }

    // Get the starting position of the end of central directory record
    $start = strpos($string, "\x50\x4b\x05\x06");

    // Error
    if($start === false)
      die('Could not find the end of central directory record');

    // Get the ecdr
    $eof_cd = substr($string, $start+4, 18);

    // Unpack the ecdr infos
    $eof_cd = unpack('vdisc1/'.
                     'vdisc2/'.
                     'ventries1/'.
                     'ventries2/'.
                     'Vsize/'.
                     'Voffset/'.
                     'vcomment_lenght', $eof_cd);

    // Do not allow multi disc zips
    if($eof_cd['disc1'] != 0)
      die('multi disk stuff is not yet implemented :/');

    // Save the interesting values
    $cd_entries = $eof_cd['entries1'];
    $cd_size    = $eof_cd['size'];
    $cd_offset  = $eof_cd['offset'];

    // Get the central directory record
    $cdr = substr($string, $cd_offset, $cd_size);

    // Reset the position and the list of the entries
    $pos     = 0;
    $entries = array();

    // Handle cdr
    while($pos < strlen($cdr)) {
      // Check header signature
      // Digital signature
      if(substr($cdr, $pos, 4) == "\x50\x4b\x05\x05") {
        // Get digital signature size
        $tmp_info = unpack('vsize', substr($cdr, $pos + 4, 2));

        // Read out the digital signature
        $digital_sig = substr($header, $pos + 6, $tmp_info['size']);

        break;
      }

      // Get file header
      $header = substr($cdr, $pos, 46);

      // Unpack the header information
      $header_info = @unpack('Vheader/'.
                             'vversion_made_by/'.
                             'vversion_needed/'.
                             'vgeneral_purpose/'.
                             'vcompression_method/'.
                             'vlast_mod_time/'.
                             'vlast_mod_date/'.
                             'Vcrc32/'.
                             'Vcompressed_size/'.
                             'Vuncompressed_size/'.
                             'vname_length/'.
                             'vextra_length/'.
                             'vcomment_length/'.
                             'vdisk_number/'.
                             'vinternal_attributes/'.
                             'Vexternal_attributes/'.
                             'Voffset',
                             $header);

      // Valid header?
      if($header_info['header'] != 33639248)
        return false;

      // New position
      $pos += 46;

      // Read out the file name
      $header_info['name'] = substr($cdr, $pos, $header_info['name_length']);

      // New position
      $pos += $header_info['name_length'];

      // Read out the extra stuff
      $header_info['extra'] = substr($cdr, $pos, $header_info['extra_length']);

      // New position
      $pos += $header_info['extra_length'];

      // Read out the comment
      $header_info['comment'] = substr($cdr, $pos, $header_info['comment_length']);

      // New position
      $pos += $header_info['comment_length'];

      // Append this file/dir to the entry list
      $entries[] = $header_info;
    }

    // Check whether all entries where read sucessfully
    if(count($entries) != $cd_entries)
      return false;

    // Handle files/dirs
    foreach($entries as $entry) {
      // Is a dir?
      if($entry['external_attributes'] & 16) {
        $this->add_dir($entry['name']);
        continue;
      }

      // Get local file header
      $header = substr($string, $entry['offset'], 30);

      // Unpack the header information
      $header_info = @unpack('Vheader/'.
                             'vversion_needed/'.
                             'vgeneral_purpose/'.
                             'vcompression_method/'.
                             'vlast_mod_time/'.
                             'vlast_mod_date/'.
                             'Vcrc32/'.
                             'Vcompressed_size/'.
                             'Vuncompressed_size/'.
                             'vname_length/'.
                             'vextra_length',
                             $header);

      // Valid header?
      if($header_info['header'] != 67324752)
        return false;

      // Get content start position
      $start = $entry['offset'] + 30 + $header_info['name_length'] + $header_info['extra_length'];

      // Get the compressed data
      $data = substr($string, $start, $header_info['compressed_size']);

      // Detect compression type
      switch($header_info['compression_method']) {
        // No compression
        case 0:
          // Ne decompression needed
          $content = $data;
          break;

        // Gzip
        case 8:
          if(!function_exists('gzinflate'))
            return false;

          // Uncompress data
          $content = gzinflate($data);
          break;

        // Bzip2
        case 12:
          if(!function_exists('bzdecompress'))
            return false;

          // Decompress data
          $content = bzdecompress($data);
          break;

        // Compression not supported -> error
        default:
          return false;
      }

      // Try to add file
      if(!$this->add_file($entry['name'], $content))
        return false;
    }

    return true;
  }
}

function &byref() {
    $x = array();
    return $x;
}

// Test highlighting of magic methods and variables
class MagicClass {
  public $magic_str;
  public $ordinary_str;

  public function __construct($some_var) {
    $this->magic_str = __FILE__;
    $this->ordinary_str = $some_var;
  }

  public function __toString() {
    return $this->magic_str;
  }

  public function nonMagic() {
    return $this->ordinary_str;
  }
}

$magic = new MagicClass(__DIR__);
__toString();
$magic->nonMagic();
$magic->__toString();

     echo <<<EOF

     Test the heredocs...

     EOF;

echo <<<"some_delimiter"
more heredoc testing
continues on this line
some_delimiter;

?>

