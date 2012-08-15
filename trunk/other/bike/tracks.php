<?php

function distance_km($lat1, $lng1, $lat2, $lng2) {
  $ra = M_PI/180; 
  $b = $lat1 * $ra;
  $c = $lat2 * $ra;
  $d = $b - $c; 
  $g = $lng1 * $ra - $lng2 * $ra;

  $tmp = sin($d/2) * sin($d/2) + cos($b) * cos($c) * sin($g/2)*sin($g/2);
  $f = 2 * asin(sqrt($tmp));
  return $f * 6378.137; 
}

$request = (object) array('query' => $_GET['query'],
                          'west' => $_GET['west'],
                          'east' => $_GET['east'],
                          'north' => $_GET['north'],
                          'south' => $_GET['south'],
                          'tracks' => $_GET['tracks']);


error_reporting(E_ALL);
try {

    $max_distance = distance_km($request->west, $request->north,
                             $request->east, $request->south) / 250.0;
    $expected_trkpoint_distance = 30 /*km/h*/ / (60*15 /* 4s interval */);
    $resolution_level = 0;
    $SEQ_RES = 1;
    while ($expected_trkpoint_distance < $max_distance / 2 &&
           $resolution_level < 6) {
        $expected_trkpoint_distance *= 2;
        $resolution_level++;
        $SEQ_RES *= 2;
    }

    $where_parts = array(
        'latitude > ?',
        'latitude < ?',
        'longitude > ?',
        'longitude < ?',
        'type != "cesta"',
        'resolution_level >= ?',
    );

    $MARGIN = 0.3; // 30% greater bounding-box

    $params = array(
        $request->south * (1 + $MARGIN) - $request->north * $MARGIN,
        $request->north * (1 + $MARGIN) - $request->south * $MARGIN,
        $request->west * (1 + $MARGIN) - $request->east * $MARGIN,
        $request->east * (1 + $MARGIN) - $request->west * $MARGIN,
        $resolution_level,
    );

    if ($request->tracks != "all") {
        $where_parts[] = "tracks.id = ?";
        $params[] = $request->tracks;
    }

    function fetch_tracks_from_db($sql_where_parts, $sql_params, $SEQ_RES) {
        $where = implode(' AND ', $sql_where_parts);

        $db = new PDO('sqlite:trips.db');
        $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

        $stmt = $db->prepare("SELECT tracks.id, tracks.date, tracks.type,
            tracks.name, tracks.full_name, segments.segment_id, sequence_no, latitude, longitude FROM 
            tracks JOIN segments on tracks.id = segments.track_id 
            JOIN points ON points.segment_id = segments.segment_id
            WHERE $where
           ORDER BY tracks.date ASC, segments.segment_id, sequence_no ASC
            ");
        $stmt->execute(
            $sql_params
            );

        $tracks = array();
        $prev_seg = -1;
        $prev_seq = -1;


        while ($row = $stmt->fetch()) {
            if (!array_key_exists($row['id'], $tracks)) {
                $tracks[$row['id']] = array(
                    'date' => $row['date'],
                    'type' => $row['type'],
                    'name' => $row['name'],
                    'full_name' => $row['full_name'],
                    'segments' => array()
                );
            }
            unset($trk);
            $trk = &$tracks[$row['id']];
            if ($prev_seg != $row['segment_id'] ||
                $prev_seq + $SEQ_RES != $row['sequence_no']) {
                unset($segment);
                $segment = array();
                $trk['segments'][] = &$segment;
            }

            $segment[] = array('lat' => $row['latitude'], 'lng' => $row['longitude']);

            $prev_seg = $row['segment_id'];
            $prev_seq = $row['sequence_no'];
        }

        return $tracks;
    };

    function tracks_add_photo_albums(&$tracks) {
        foreach ($tracks as &$trk) {
            $year = substr($trk['date'], 0, 4);
            $full_name = substr($trk['full_name'],0,-4);
            $ALBUMS_LOCAL_PATH="/home/ppershing/public_html/data/foto/gal";
            //echo "$ALBUMS_LOCAL_PATH/$year/$full_name<br/>";
            if (is_dir("$ALBUMS_LOCAL_PATH/$year/$full_name")) {
                $ALBUMS_URL="http://people.ksp.sk/~ppershing/foto/index.php?spgmGal=";
                $trk['photo_album'] = $ALBUMS_URL."$year/$full_name";
            } else {
                $trk['photo_album'] = null;
            }
        }
    }

    function tracks_compact_segments(&$tracks, $resolution_level, $max_distance) {
        foreach ($tracks as &$trk) {
            $trk['seg'] = array();
            foreach ($trk['segments'] as $trkseg) {
                $distance = 0;
                $lastpt = $trkseg[0];
                $selection = array($trkseg[0]);
                foreach ($trkseg as $trkpt) {
                    $distance += distance_km($lastpt["lat"], $lastpt["lon"],
                                          $trkpt["lat"], $trkpt["lon"]);
                    if ($resolution_level == 0 || $distance >= $max_distance) {
                        $selection[] = $trkpt;
                        $distance -= $max_distance;
                    }

                    $lastpt = $trkpt;
                }
                $selection[] = $trkseg[count($trkseg)-1];
                $trk['seg'][] = $selection;
            }
            unset($trk['segments']);
        }
    }

    $tracks = fetch_tracks_from_db($where_parts, $params, $SEQ_RES);

    tracks_add_photo_albums($tracks);
    tracks_compact_segments($tracks, $resolution_level, $max_distance);

    echo json_encode($tracks);
} catch(PDOException $e) {
    echo $e;
}
