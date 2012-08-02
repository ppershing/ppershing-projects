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
    $db = new PDO('sqlite:trips.db');
    $db->setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);

    $where_parts = array(
        'latitude > ?',
        'latitude < ?',
        'longitude > ?',
        'longitude < ?',
        'type != "cesta"',
        'low_res = 1',
    );

    $MARGIN = 0.3; // 30% greater bounding-box

    $params = array(
        $request->south * (1 + $MARGIN) - $request->north * $MARGIN,
        $request->north * (1 + $MARGIN) - $request->south * $MARGIN,
        $request->west * (1 + $MARGIN) - $request->east * $MARGIN,
        $request->east * (1 + $MARGIN) - $request->west * $MARGIN,
    );

    if ($request->tracks != "all") {
        $where_parts[] = "tracks.id = ?";
        $params[] = $request->tracks;
    }

    $where = implode(' AND ', $where_parts);

    $stmt = $db->prepare("SELECT tracks.id, tracks.date, tracks.type,
        tracks.name, segments.segment_id, sequence_no, latitude, longitude FROM 
        tracks JOIN segments on tracks.id = segments.track_id 
        JOIN points ON points.segment_id = segments.segment_id
        WHERE $where
       ORDER BY tracks.date ASC, segments.segment_id, sequence_no ASC
        ");
    $stmt->execute(
        $params
        );

    $tracks = array();
    $prev_seg = -1;
    $prev_seq = -1;

    $SEQ_RES=15; // keep in sync with python

    while ($row = $stmt->fetch()) {
        if (!array_key_exists($row['id'], $tracks)) {
            $tracks[$row['id']] = array(
                'date' => $row['date'],
                'type' => $row['type'],
                'name' => $row['name'],
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
    unset($trk);
    unset($segment);

    $max_distance = distance_km($request->west, $request->north,
                             $request->east, $request->south) / 500.0;
    foreach ($tracks as &$trk) {
        $trk['seg'] = array();
        foreach ($trk['segments'] as $trkseg) {
            $distance = 0;
            $lastpt = $trkseg[0];
            $selection = array($trkseg[0]);
            foreach ($trkseg as $trkpt) {
                $distance += distance_km($lastpt["lat"], $lastpt["lon"],
                                      $trkpt["lat"], $trkpt["lon"]);
                if ($distance >= $max_distance) {
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
    echo json_encode($tracks);
} catch(PDOException $e) {
    echo $e;
}
