#' PredPsych.
#'
#' @name PredPsych
#' @docType package
#' @details 
#' "PredPsych" is a user-friendly, R toolbox based on machine learning predictive algorithms.
#' 
#' @author 
#' Atesh Koul, C'MON unit, Istituto Italiano di Tecnologia
#' 
#' \email{atesh.koul@@iit.it}
#' 
#' @references 
#' Koul, A., Becchio, C., & Cavallo, A. (2017, March 21). 
#' PredPsych: A toolbox for predictive machine learning based approach in experimental 
#' psychology research. Retrieved from osf.io/preprints/psyarxiv/pvjac
#' 
NULL

#' Kinematics Dataset
#' A dataset containing part of the motion capture dataset freely available 
#' in the publication (Ansuini et al., 2015).The dataset was obtained by 
#' recording 15 naive participants performing reach-to-grasp movements towards 
#' two differently sized objects:  a small object (i.e., hazelnut) and a 
#' large object (i.e., grapefruit). The variables are as follows:
#'
#' \itemize{
#'   \item Object Size :                              Size of the to-be-grasped object
#'   (1 = small, 2 = large)
#'   
#'   \item Wrist_Velocity_01 .. Wrist_Height_10:      module of the velocity of the wrist marker
#'   (mm/sec) from 10\% (_01) to 100\% (_10) of the movement
#'   
#'   \item Grip_Aperture_01 .. Grip_Aperture_10       Distance between the marker placed 
#'   on thumb tip and that placed on the tip of the index finger (mm) from 10\% (_01) 
#'   to 100\% (_10) of the movement
#'   
#'   \item Wrist_Height_01 .. Wrist_Height_10         z-component of the wrist marker (mm)
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   
#'   \item x_index_01 .. x_index_10 :                 x-coordinates for the index with respect to F-local (mm) 
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   \item y_index_01 .. y_index_10 :                 y-coordinates for the index with respect to F-local (mm) 
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   \item z_index_01 .. z_index_10 :                 z-coordinates for the index with respect to F-local (mm) 
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   
#'   \item x_thumb_01 .. x_thumb_10 :                 x-coordinates for the thumb with respect to F-local (mm) 
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   \item y_thumb_01 .. y_thumb_10 :                 y-coordinates for the thumb with respect to F-local (mm) 
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   \item z_thumb_01 .. z_thumb_10 :                 z-coordinates for the thumb with respect to F-local (mm) 
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   
#'   \item x_finger_plane_01 .. x_finger_plane_10     x-components of the thumb-index plane
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   \item y_finger_plane_01 .. y_finger_plane_10     y-components of the thumb-index plane
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   \item z_finger_plane_01 .. z_finger_plane_10     z-components of the thumb-index plane
#'   from 10\% (_01) to 100\% (_10) of the movement
#'   
#' }
#'
#' @docType data
#' @keywords datasets
#' @name KinData
#' @usage data(KinData)
#' @format A data frame with 848 rows and 121 variables
NULL